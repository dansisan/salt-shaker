module Main exposing (..)

import Html exposing (..)
import Autocomplete
import Html.Attributes exposing (..)
import FoodSelector

-- MODEL

type alias Model =
  { foodSelectorModel : FoodSelector.Model
  }


initialModel : Model
initialModel = { foodSelectorModel = FoodSelector.init
               }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- MESSAGES

type Msg
  = AutocompleteMsg FoodSelector.Msg

-- VIEW

view: Model -> Html Msg
view model =

  Html.div []
      [ Html.map AutocompleteMsg (FoodSelector.view model.foodSelectorModel)
      , div [style [ ("position", "relative"), ("text-align", "center"), ("top", "250px"), ("font-size", "24px") ]]
            [ getFoodDisplay model ]
      ]

getFoodDisplay : Model -> Html msg
getFoodDisplay model =
    case model.foodSelectorModel.selectedFood of
        Nothing ->
            div [] [ text "" ]
        Just food ->
            div []
                [ text ( food.name ++ " has " )
                , span [style [("font-weight", "bold")] ] [ text ( toString food.salt ++ "mg" )  ]
                , text ( " of salt " ) ]

-- UPDATE

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AutocompleteMsg subMsg ->
      let
        ( foodSelectorModel, autocompleteCmd ) =
            FoodSelector.update subMsg model.foodSelectorModel
      in
        ({ model | foodSelectorModel = foodSelectorModel }, Cmd.map AutocompleteMsg autocompleteCmd )


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
            -- process module subscriptions
            [ Sub.map AutocompleteMsg (FoodSelector.subscriptions model.foodSelectorModel)
            ]
-- MAIN
main: Program Never Model Msg
main =
  program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }