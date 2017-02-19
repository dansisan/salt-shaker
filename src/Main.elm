module Main exposing (..)

import Html exposing (..)
import Autocomplete
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Animation exposing (px)
import FoodSelector

-- MODEL

type alias Model =
  { foodSelectorModel : FoodSelector.Model
  , animationStyle : Animation.State
  }


initialModel : Model
initialModel = { foodSelectorModel = FoodSelector.init
               , animationStyle = Animation.style
                                    [ Animation.translate (px 0.0) (px 0.0)
                                    , Animation.opacity 1.0
                                    ]
               }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- MESSAGES

type Msg
  = AutocompleteMsg FoodSelector.Msg
    | Animate Animation.Msg
    | ShakeIt

-- VIEW

view: Model -> Html Msg
view model =

  Html.div []
      [ Html.map AutocompleteMsg (FoodSelector.view model.foodSelectorModel)
      , img [ style [ ("position", "absolute" ), ("transform", "rotate(180deg)") ], src "../salt-shaker.jpg" ] []
      , div [style [ ("position", "relative"), ("text-align", "center"), ("top", "250px"), ("font-size", "24px") ]]
            [ getFoodDisplay model ]
      , div
            (Animation.render model.animationStyle
                ++ [ onClick ShakeIt
                   , style
                        [ ( "position", "absolute" )
                        , ( "margin", "100px auto" )
                        , ( "padding", "25px" )
                        , ( "width", "200px" )
                        , ( "height", "200px" )
                        , ( "background-color", "#268bd2" )
                        , ( "color", "white" )
                        , ( "top", "305px" )
                        ]
                   ]
            )
            [ text "Click to Animate!" ]
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

    Animate animMsg ->
                ( { model
                    | animationStyle = Animation.update animMsg model.animationStyle
                  }
                , Cmd.none
                )

    ShakeIt ->
                ( { model | animationStyle =
                        Animation.interrupt
                            [ Animation.to
                                [ Animation.translate (px 0) (px 100) ]
                            , Animation.to
                                [ Animation.translate (px 0) (px 0) ]
                            ]
                            model.animationStyle
                  }
                , Cmd.none
                )


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
            -- process module subscriptions
            [ Sub.map AutocompleteMsg (FoodSelector.subscriptions model.foodSelectorModel)
            , Animation.subscription Animate [ model.animationStyle ]
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