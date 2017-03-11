module FoodSelector exposing (..)

import Autocomplete
import Char
import Csv
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Html.Events exposing (..)
import String
import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE
import Dict exposing (..)
import Dom
import Task


main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription


type alias Model =
    { foods : List Food
    , autoState : Autocomplete.State
    , howManyToShow : Int
    , query : String
    , activeMenuFood : Maybe Food -- currently active in menu and will be selected on enter/mouse
    , showMenu : Bool
    , selectedFood : Maybe Food -- selected by hitting enter or mouse
    }


init : Model
init =
    { foods = [ Food "" [] "" ] -- later populated from csv
    , autoState = Autocomplete.empty
    , howManyToShow = 12
    , query = ""
    , activeMenuFood = Nothing
    , showMenu = False
    , selectedFood = Nothing
    }


type Msg
    = SetQuery String
    | SetAutoState Autocomplete.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectFoodKeyboard String
    | SelectFoodMouse String
    | PreviewFood String
    | OnFocus
    | LoadFoods (Result Http.Error String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| (acceptableFood newQuery model.foods)
            in
                { model | query = newQuery, showMenu = showMenu, activeMenuFood = Nothing } ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState (acceptableFood model.query model.foods)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (acceptableFood model.query model.foods)

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        { model | query = "" }
                            |> removeSelection
                            |> resetMenu

                escapedModel =
                    case model.activeMenuFood of
                        Just food ->
                            if model.query == food.name then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
                escapedModel ! []

        Wrap toTop ->
            case model.activeMenuFood of
                Just food ->
                    update Reset model

                Nothing ->
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem updateConfig (acceptableFood model.query model.foods) model.howManyToShow model.autoState
                            , activeMenuFood = List.head <| List.reverse <| List.take model.howManyToShow <| (acceptableFood model.query model.foods)
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (acceptableFood model.query model.foods) model.howManyToShow model.autoState
                            , activeMenuFood = List.head <| List.take model.howManyToShow <| (acceptableFood model.query model.foods)
                        }
                            ! []

        Reset ->
            { model | autoState = Autocomplete.reset updateConfig model.autoState, activeMenuFood = Nothing } ! []

        SelectFoodKeyboard id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
                {newModel | selectedFood = model.activeMenuFood} ! []

        SelectFoodMouse id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
                ( {newModel | selectedFood = model.activeMenuFood}, Task.attempt (\_ -> NoOp) (Dom.focus "food-input") )

        PreviewFood id ->
            { model | activeMenuFood = Just <| getFoodAtId model.foods id } ! []

        OnFocus ->
            model ! []

        LoadFoods (Ok foodString) ->
            { model | foods = processCsv ( Csv.split foodString ) } ! []

        LoadFoods (Err _) ->
             { model | foods = [ Food "" [] "" ] } ! []

        NoOp ->
            model ! []


resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu


removeSelection model =
    { model | activeMenuFood = Nothing }


getFoodAtId foods id =
    List.filter (\food -> food.name == id) foods
        |> List.head
        |> Maybe.withDefault (Food "" [] "")


setQuery model id =
    { model
        | query = .name <| getFoodAtId model.foods id
        , activeMenuFood = Just <| getFoodAtId model.foods id
    }


resetMenu model =
    { model
        | autoState = Autocomplete.empty
        , showMenu = False
    }


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.Decode.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.Decode.andThen
                    fromResult

        fromResult : Result String a -> Json.Decode.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.Decode.succeed val

                Err reason ->
                    Json.Decode.fail reason

        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []

        query =
            case model.activeMenuFood of
                Just food ->
                    food.name

                Nothing ->
                    model.query

        activeDescendant attributes =
            case model.activeMenuFood of
                Just food ->
                    (attribute "aria-activedescendant"
                        food.name
                    )
                        :: attributes

                Nothing ->
                    attributes
    in
        div [ class "parent" ]
            (List.append
                [ input
                    (activeDescendant
                        [ onInput SetQuery
                        , onFocus OnFocus
                        , onWithOptions "keydown" options dec
                        , value query
                        , id "food-input"
                        , class "autocomplete-input"
                        , autocomplete False
                        , attribute "aria-owns" "list-of-foods"
                        , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                        , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                        , attribute "role" "combobox"
                        , attribute "aria-autocomplete" "list"
                        , attribute "autofocus" "true"
                        ]
                    )
                    []
                ]
                menu
            )

formatName : String -> String
formatName input =
    let
        capitalizeWord : String -> String
        capitalizeWord input =
            case String.uncons input of
                Just (c, tl) -> (String.toUpper (String.fromChar c)) ++ String.toLower tl
                Nothing -> input

        ensureFirstIsCapital : String -> String
        ensureFirstIsCapital input =
            case String.uncons input of
                Nothing -> input
                Just (c, tl) -> (String.toUpper (String.fromChar c)) ++ tl

        capitalizeIfUpper : String -> String
        capitalizeIfUpper input =
            if String.all isUpperLike input then capitalizeWord input else input

        isUpperLike : Char -> Bool
        isUpperLike c =
            if (c == '-') || (c == ',') || (c == '\'') then True
                else Char.isUpper c

        words = String.split " " ( ensureFirstIsCapital input )
    in
        String.join " " (List.map capitalizeIfUpper words)

-- Used with foldl to group SubFoods
-- SubFood is everything after the first comma in the name
groupSubFoods : List String -> Dict String (List SubFood) -> Dict String (List SubFood)
groupSubFoods csvCols dict =
    let
       badMatchEntry =  (("", ""), "", 0, "")
       ((foodName, subFoodName), serving, salt, source) = case csvCols of
            [] -> badMatchEntry
            _ :: [] -> badMatchEntry
            _ :: _ :: [] -> badMatchEntry
            _ :: _ :: _ :: [] -> badMatchEntry
            [ foodCol, serving, salt, source ] -> ( (unpackFoodCol foodCol), serving, ( Result.withDefault 0 (String.toInt salt) ), source)
            _ :: _ :: _ :: _ -> badMatchEntry
       key = foodName
       subFood = SubFood subFoodName serving salt
       existingValue = Maybe.withDefault [] (Dict.get key dict)
       newValue = List.append existingValue [subFood]
    in
       Dict.insert key newValue dict

processCsv : List (List String) -> List Food
processCsv csv =
    let dict = Dict.fromList []
    in
        List.map makeFood (List.foldl groupSubFoods dict csv |> Dict.toList)

makeFood : (String, List SubFood) -> Food
makeFood (foodName, subFoods) =
    Food (formatName foodName) subFoods ""

-- Split food name column into a higher level food name, which appears in search
-- and a SubFood name, which is a variety and will be grouped in a dropdown
unpackFoodCol : String -> (String, String)
unpackFoodCol firstField =
        case String.split "," firstField of
            h :: tl -> if List.length tl == 0
                then (h,h) -- use full string/name as subname
                else (h, String.join "," tl)
            _ -> ("", "")

acceptableFood : String -> List Food -> List Food
acceptableFood query foods =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .name) foods


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptableFood model.query model.foods)) ]


updateConfig : Autocomplete.UpdateConfig Msg Food
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewFood maybeId
                else if code == 13 then
                    Maybe.map SelectFoodKeyboard maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewFood id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectFoodMouse id
        , separateSelections = False
        }


viewConfig : Autocomplete.ViewConfig Food
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id person.name
                ]
            , children = [ Html.text person.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }



-- FOOD

type alias Food =
    { name : String
    , subFoods : List SubFood
    , source : String
    }

type alias SubFood =
    { subname : String
    , serving : String
    , salt : Int
    }

-- CSV feed
-- "https://docs.google.com/spreadsheets/d/1pis8-nvG4uhutYepv__-MSDUQIqch_45fgc1h6fSIfs/export?exportFormat=csv&amp;gid=0"

foods : List Food
foods = List.map getFood ( Csv.split """Beans baked can,1114\nMcBiscuit with Egg and Sausage,1141\nCheeseburger,891""" )

getCsv : Cmd Msg
getCsv =
    Http.send LoadFoods ( Http.getString "https://gist.githubusercontent.com/dansisan/d657c2e7a36b3b390449821ecc33825b/raw/food-salt.csv" )

-- Have to use this with gulp, not reactor, which requires Internet
--    Http.send LoadFoods ( Http.getString "/usda.csv" )

-- Dummy record with the err in place of the name
nullFood : String -> Food
nullFood err = { name = err, subFoods = [ { subname = "", serving = "", salt = 0 } ] , source = "" }

nullSubFood : SubFood
nullSubFood = { subname = "", serving = "", salt = 0 }

-- UPDATE: Not needed now that lovasoa fixed bug in Csv.split
-- Simpler API for recursive method below
getParsedCsvLine : String -> List String
getParsedCsvLine input =
    parseCsvLine input False "" []

-- Splits by comma, respects quote escaping, and removes double quotes
parseCsvLine : String -> Bool -> String -> List String -> List String
parseCsvLine input isQuoteOpen currentEntry pastResults =
    case ((String.uncons input), isQuoteOpen) of
    (Nothing, _) -> pastResults ++ [currentEntry]
    (Just ('\"', tl), _)  -> parseCsvLine tl (not isQuoteOpen) currentEntry pastResults
    (Just (',', tl), True) -> parseCsvLine tl True (currentEntry ++ ",") pastResults
    (Just (',', tl), False) -> parseCsvLine tl False "" (pastResults ++ [currentEntry])
    (Just (c, tl), _) -> parseCsvLine tl (isQuoteOpen) (currentEntry ++ String.fromChar c) pastResults