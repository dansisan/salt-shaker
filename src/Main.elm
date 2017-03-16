module Main exposing (..)

import Html exposing (..)
import Http
import Autocomplete
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode
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
            [ viewFood model
            , viewSalt model.foodSelectorModel.selectedSubFood
            ]
      ]

getNumShakes : Model -> Int
getNumShakes model =
    case model.foodSelectorModel.selectedSubFood of
        Nothing ->
            0
        Just subFood ->
            shakesFromMg subFood.salt


-- From salt package, .54 g sodium / 1.4 g salt = .386
-- Exp 1, 3 holes open, 74 shakes / 4 g = 48
-- Exp 2, 1 hole open, 80 shakes / 3 g = 69
-- Avg 58.5

shakesFromMg : Int -> Int
shakesFromMg mg = mg *  59 // 1000

viewFood : Model -> Html Msg
viewFood model =
    case model.foodSelectorModel.selectedFood of
        Nothing ->
            div [] [ text "" ]
        Just food ->
            div []
                [ span [style [("font-weight", "bold")] ] [ text food.name ]
                , viewSubFoodDropdown food.subFoods
                ]

viewSalt : Maybe SubFood -> Html Msg
viewSalt subFood =
    case subFood of
        Just subFood -> div [] [ text (toString subFood.salt ++ "mg of salt. That's " ++ toString (shakesFromMg subFood.salt) ++ " shakes!")
                               , viewSource subFood.source
                               ]
        Nothing -> span [] []

viewSubFoodDropdown : List SubFood -> Html Msg
viewSubFoodDropdown subFoods =
    div [] [ select [ onInput SetSubFood ] (List.map viewSubFoodOption subFoods) ]


viewSubFoodOption : SubFood -> Html Msg
viewSubFoodOption subFood =
    option [ Html.Attributes.value (subFoodToJson subFood) ] [ text (subFood.subname ++ " (" ++ subFood.serving ++ ")")]


viewSource : String -> Html msg
viewSource source =
    let sourceOrDefault = if String.isEmpty source then "USDA" else source

    in
        div[ style [("font-size", "50%"), ("color", "gray")] ] [ text ( "Source: " )
              , if String.startsWith "http" sourceOrDefault then a [ href sourceOrDefault ] [ text (sourceOrDefault) ] else text sourceOrDefault
              ]


-- Serialize SubFood

subFoodToJson : SubFood -> String
subFoodToJson subFood =
    let
        requiredFields = [ ("salt", Json.Encode.int subFood.salt) ]
        -- only add source to Json if not empty -- change source to be a Maybe String
        fields = if String.isEmpty subFood.source
                    then requiredFields
                    else ("source", Json.Encode.string subFood.source) :: requiredFields
    in
        Json.Encode.encode 0 (
            Json.Encode.object
                fields
        )

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

    SetSubFood json ->
        let
          ( foodSelectorModel, cmd ) =
                FoodSelector.update (FoodSelector.SetSubFood json) model.foodSelectorModel
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
