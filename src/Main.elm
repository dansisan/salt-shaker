module Main exposing (..)

import Html exposing (..)
import Http
import Autocomplete
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Animation exposing (px, turn)
import Ease exposing (..)
import FoodSelector exposing (..)

-- MODEL

type alias Model =
  { foodSelectorModel : FoodSelector.Model
  , animationStyle : Animation.State
  }


initialModel : Model
initialModel = { foodSelectorModel = FoodSelector.init
               , animationStyle = Animation.styleWith (Animation.easing { duration = 224.0, ease = bezier 0.94 0.01 0.94 0.44 })
                                    [ Animation.translate (px 0.0) (px 0.0)
                                    , Animation.rotate (turn 0) ]
               }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map LoadFoods FoodSelector.getCsv )

-- MESSAGES

type Msg
  = AutocompleteMsg FoodSelector.Msg
    | Animate Animation.Msg
    | ShakeIt Int
    | LoadFoods FoodSelector.Msg
    | SetSubFood String

-- VIEW

view: Model -> Html Msg
view model =

  Html.div []
      [ Html.map AutocompleteMsg (FoodSelector.view model.foodSelectorModel)
      , img
            (Animation.render model.animationStyle
                ++ [ onClick (ShakeIt (getNumShakes model))
                   , style
                        [ ( "position", "absolute" )
                        ]
                    , src "../salt-shaker.jpg"
                   ]
            ) []

      , div [style [ ("position", "relative"), ("text-align", "center"), ("top", "250px"), ("font-size", "24px") ]]
            [ getFoodDisplay model
            , getSaltDisplay model.foodSelectorModel.selectedSubFoodMg
            ]
      ]

getNumShakes : Model -> Int
getNumShakes model =
    case model.foodSelectorModel.selectedSubFoodMg of
        Nothing ->
            0
        Just mg ->
            shakesFromMg mg


-- From salt package, .54 g sodium / 1.4 g salt = .386
-- Exp 1, 3 holes open, 74 shakes / 4 g = 48
-- Exp 2, 1 hole open, 80 shakes / 3 g = 69
-- Avg 58.5

shakesFromMg : Int -> Int
shakesFromMg mg = mg *  59 // 1000

getFoodDisplay : Model -> Html Msg
getFoodDisplay model =
    case model.foodSelectorModel.selectedFood of
        Nothing ->
            div [] [ text "" ]
        Just food ->
            div []
                [ span [style [("font-weight", "bold")] ] [ text food.name ]
                , displaySubFoods food.subFoods
                , getSource food.source
                ]

getSaltDisplay : Maybe Int -> Html Msg
getSaltDisplay mg =
    case mg of
        Just mg -> div [] [ text (toString mg ++ "mg of salt. That's " ++ toString (shakesFromMg mg) ++ " shakes!") ]
        Nothing -> span [] []

displaySubFoods : List SubFood -> Html Msg
displaySubFoods subFoods =
    div [] [ select [ onInput SetSubFood ] (List.map subFoodOption subFoods) ]


subFoodOption : SubFood -> Html Msg
subFoodOption subFood =
    option [ Html.Attributes.value (toString subFood.salt) ] [ text (subFood.subname ++ " (" ++ subFood.serving ++ ")")]


getSource : String -> Html msg
getSource source =
    if String.isEmpty source
        then text ""
    else
        div[] [ text ( "Source: " )
              , if String.startsWith "http" source then a [ href source ] [ text (source) ] else text source
              ]

-- UPDATE

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AutocompleteMsg subMsg ->
      let
        ( foodSelectorModel, autocompleteCmd ) =
            FoodSelector.update subMsg model.foodSelectorModel
      in
        ( { model | foodSelectorModel = foodSelectorModel }, Cmd.map AutocompleteMsg autocompleteCmd )

    Animate animMsg ->
        ( { model | animationStyle = Animation.update animMsg model.animationStyle }, Cmd.none )

    ShakeIt nTimes ->
        ( { model | animationStyle =
            Animation.interrupt
                [ Animation.to
                    [ Animation.rotate (turn 0.5) ]
                , ( Animation.repeat nTimes
                    [ Animation.to
                        [ Animation.translate (px 0) (px 150) ]
                    , Animation.to
                        [ Animation.translate (px 0) (px 0) ]
                    ] )
                , Animation.to
                    [ Animation.rotate (turn 0.0) ]
                ]
            model.animationStyle
          }
        , Cmd.none
        )

    LoadFoods subMsg ->
        let
          ( foodSelectorModel, loadFoodsCmd ) =
                FoodSelector.update subMsg model.foodSelectorModel
        in
          ({ model | foodSelectorModel = foodSelectorModel }, Cmd.none )

    SetSubFood mg ->
        let
          ( foodSelectorModel, cmd ) =
                FoodSelector.update (FoodSelector.SetSubFood mg) model.foodSelectorModel
        in
          ( {model | foodSelectorModel = foodSelectorModel}, Cmd.none )

onChange : (Int -> msg) -> Html.Attribute msg
onChange handler =
    Html.Events.on "change" <| Json.Decode.map handler <| Json.Decode.at ["target", "value"] Json.Decode.int

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
