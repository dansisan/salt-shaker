module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import AutocompleteModule

-- MODEL

type alias Model =
  { autocompleteModel : AutocompleteModule.Model }


initialModel : Model
initialModel = { autocompleteModel = AutocompleteModule.init }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- MESSAGES

type Msg
  = AutocompleteMsg AutocompleteModule.Msg

-- VIEW

view: Model -> Html Msg
view model =

  Html.div []
      [ Html.map AutocompleteMsg (AutocompleteModule.view model.autocompleteModel)
      ]

-- UPDATE

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AutocompleteMsg subMsg ->
      let
        ( autocompleteModel, autocompleteCmd ) =
            AutocompleteModule.update subMsg model.autocompleteModel
      in
        ({ model | autocompleteModel = autocompleteModel }, Cmd.map AutocompleteMsg autocompleteCmd )


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.none

-- MAIN
main: Program Never Model Msg
main =
  program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }