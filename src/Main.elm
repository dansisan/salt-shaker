module Main exposing (..)

import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Decode as Json
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
    { foods = foods
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
                ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "food-input") )

        PreviewFood id ->
            { model | activeMenuFood = Just <| getFoodAtId model.foods id } ! []

        OnFocus ->
            model ! []

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
        |> Maybe.withDefault (Food "" 0)


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
            (Json.map
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
                |> Json.andThen
                    fromResult

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason

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
                [ div [style
                           [ ("position", "absolute")
                           , ("top", "300px")
                           ]
                       ] [ text (getFoodDisplay model) ] ]
            )


getFoodDisplay : Model -> String
getFoodDisplay model =
    case model.selectedFood of
        Nothing ->
            ""
        Just food ->
            food.name ++ " has " ++ toString food.salt ++ " mg of salt "


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
    , salt : Int
    }

foods : List Food
foods =
    [ Food "Beans, baked, can" 1114
    , Food "McBiscuit with Egg and Sausage" 1141
    , Food "Cheese Burger" 891
    , Food "Coleslaw" 267
    , Food "Hotdog" 670
    , Food "Potato chips" 213
    , Food "Chicken noodle soup" 1106
    , Food "Small McDonald's french fries" 160
    ]