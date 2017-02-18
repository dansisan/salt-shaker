module Main exposing (..)

import Html exposing (..)
import Autocomplete
import Html.Attributes exposing (..)
import FoodSelector

-- MODEL

type alias Model =
  { autocompleteModel : FoodSelector.Model
  }


initialModel : Model
initialModel = { autocompleteModel = FoodSelector.init
               , selectedFood = Nothing
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
      [ Html.map AutocompleteMsg (FoodSelector.view model.autocompleteModel)
      ]

-- UPDATE

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AutocompleteMsg subMsg ->
      let
        ( autocompleteModel, autocompleteCmd ) =
            FoodSelector.update subMsg model.autocompleteModel
      in
        ({ model | autocompleteModel = autocompleteModel }, Cmd.map AutocompleteMsg autocompleteCmd )


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
            -- process module subscriptions
            [ Sub.map AutocompleteMsg (FoodSelector.subscriptions model.autocompleteModel)
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