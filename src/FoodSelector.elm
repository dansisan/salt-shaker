module FoodSelector exposing (..)

import Autocomplete
import Csv
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Html.Events exposing (..)
import String
import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as JE
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
    { foods = [ Food "" "" 0 ] -- later populated from csv
    , autoState = Autocomplete.empty
    , howManyToShow = 5
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
            { model | foods = List.map getFood ( Csv.split foodString ) } ! []

        LoadFoods (Err _) ->
             { model | foods = [ Food "" "" 0 ] } ! []

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
        |> Maybe.withDefault (Food "" "" 0)


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


getFood : List String -> Food
getFood list =
  case list of
    [] -> nullFood ""
    _ :: [] -> nullFood ""
    _ :: _ :: [] -> nullFood ""
    [ name, serving, salt ] -> Food name serving ( Result.withDefault 0 (String.toInt salt) )
    _ :: _ :: _ :: _ -> nullFood ""

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
    , serving: String
    , salt : Int
    }

-- JSON feed
-- https://spreadsheets.google.com/feeds/list/1pis8-nvG4uhutYepv__-MSDUQIqch_45fgc1h6fSIfs/od6/public/values?alt=json

-- CSV feed
-- "https://docs.google.com/spreadsheets/d/1pis8-nvG4uhutYepv__-MSDUQIqch_45fgc1h6fSIfs/export?exportFormat=csv&amp;gid=0"

foods : List Food
--foods =
--    [ Food "Beans, baked, can" 1114
--    , Food "McBiscuit with Egg and Sausage" 1141
--    , Food "Cheese Burger" 891
--    , Food "Coleslaw" 267
--    , Food "Hotdog" 670
--    , Food "Potato chips" 213
--    , Food "Chicken noodle soup" 1106
--    , Food "Small McDonald's french fries" 160
--    ]

foods = List.map getFood ( Csv.split """Beans baked can,1114\nMcBiscuit with Egg and Sausage,1141\nCheeseburger,891""" )

getCsv : Cmd Msg
getCsv =
    Http.send LoadFoods ( Http.getString "https://gist.githubusercontent.com/dansisan/d657c2e7a36b3b390449821ecc33825b/raw/food-salt.csv" )

-- Dummy record with the err in place of the name
nullFood : String -> Food
nullFood err = { name = err, serving = "", salt = 0 }

getResult : String -> List Food
getResult inputJson =
      let result = Json.Decode.decodeString
            (Json.Decode.list foodDecoder)
            inputJson
      in
        case result of
            Ok val -> val
            Err err -> [ nullFood err ]

foodDecoder : Decoder Food
foodDecoder =
  decode Food
    |> Json.Decode.Pipeline.required "name" string
    |> Json.Decode.Pipeline.required "serving" string
    |> Json.Decode.Pipeline.required "salt" int

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