module Main where

import Random as R

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import String exposing (..)

import StartApp.Simple as StartApp

--MODEL

type alias Model =
  { factorA : Int
  , factorB : Int
  , input : String
  , product : Int
  , seed : R.Seed
  , isCorrect : Bool
  , isAnswered : Bool
  }


initialFactorA : Int
initialFactorA = fst <| genRand <| R.initialSeed 10000


initialFactorB : Int
initialFactorB = fst <| genRand <| R.initialSeed 10001


initialSeed : R.Seed
initialSeed = R.initialSeed 10020


initialModel : Model
initialModel =
  { factorA = initialFactorA
  , factorB = initialFactorB
  , input = ""
  , product = initialFactorA * initialFactorB
  , seed = initialSeed
  , isCorrect = False
  , isAnswered = False
  }


-- UPDATE

type Action
  = UpdateInput String
  | SubmitAnswer
  | GiveUp
  | NextQuestion

randomFactor : R.Seed -> Int
randomFactor s = fst <| genRand s


genRand : R.Seed -> (Int, R.Seed)
genRand s = s |> R.generate (R.int 0 10)


toInt : String -> Int
toInt c =
  case String.toInt c of
    Ok n -> n
    otherwise -> 0


update : Action -> Model -> Model
update action model =
  case action of
    UpdateInput s ->
      { model | input <- s }
    SubmitAnswer ->
      let
        correct = (toInt  model.input) == model.product
      in
        { model | isCorrect <- correct, isAnswered <- True }
    GiveUp ->
      { model | isAnswered <- True }
    NextQuestion ->
      let
        newRandA = genRand model.seed
        newFactA = fst newRandA
        newRandB = genRand <| snd newRandA
        newFactB = fst newRandB
        newSeed = snd newRandB
      in
        { factorA = newFactA
        , factorB = newFactB
        , input = ""
        , product = newFactA * newFactB
        , seed = newSeed
        , isCorrect = False
        , isAnswered = False
        }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.isAnswered of
    True ->
      successBox address model
    False ->
      flashCard address model


successBox : Signal.Address Action -> Model -> Html
successBox address model =
  case model.isCorrect of
    True ->
      div [ class "correct-answer" ]
          [ text "GOOD JOB!"
          , nextButton address
          ]
    False ->
      div [ class "wrong-answer" ]
          [ text "Sorry"
          , span [] [ text <| "The correct answer is" ++ (toString model.product) ]
          , nextButton address
          ]

nextButton : Signal.Address Action -> Html
nextButton address =
  button [ onClick address NextQuestion ]
         [ text "Next" ]


flashCard : Signal.Address Action -> Model -> Html
flashCard  address model =
  div [ class "flash-card" ]
      [ div [ class "operator" ]
            [ text "x" ]
      , div [ class "factors" ]
            [ h2 [] [ text <| toString model.factorA ]
            , h2 [] [ text <| toString model.factorB ]
            ]
      , div [ class "answer" ]
            [ input [ id "search"
                    , on "input" targetValue (\s -> Signal.message address (UpdateInput s))
                    , autofocus True
                    ]
                    []
            ]
      , button [ onClick address SubmitAnswer]
               [ text "Submit" ]
      , button [ onClick address SubmitAnswer]
               [ text "Give Up" ]
      ]

main : Signal Html
main = StartApp.start { view = view, update = update, model = initialModel }
