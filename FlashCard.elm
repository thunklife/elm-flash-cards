module FlashCard where

import Random as R

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import String exposing (..)

type Operator
  = Add
  | Sub
  | Mul
  | Div

type alias Model =
  { numA : Int
  , numB : Int
  , input : String
  , answer : Int
  , operator : Operator
  , seed : R.Seed
  , isCorrect : Bool
  , isAnswered : Bool
  }


genRand : R.Seed -> (Int, R.Seed)
genRand s = s |> R.generate (R.int 0 10)


numA : Int
numA = fst <| genRand <| R.initialSeed 10000


numB : Int
numB = fst <| genRand <| R.initialSeed 10001


initialSeed : R.Seed
initialSeed = R.initialSeed 10020

newRands : Operator -> R.Seed -> (Int, Int, R.Seed)
newRands op seed =
  let
    (a, s) = genRand seed
    (b, s') = genRand s
  in
    case op of
      Add -> (a, b, s')
      Mul -> (a, b, s')
      otherwise ->
        if | a >= b -> (a, b, s')
           | a < b -> (b, a, s')


solve : Operator -> (Int -> Int -> Int)
solve op =
  case op of
    Add -> (+)
    Mul -> (*)
    Sub -> (-)
    Div -> (//)


init : Operator -> Model
init op =
  let
    (a, b, s) = newRands op initialSeed 
  in
    { numA = a
    , numB = b
    , input = ""
    , answer = (solve op) a b
    , operator = op
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
        correct = (toInt  model.input) == model.answer
      in
        { model | isCorrect <- correct, isAnswered <- True }
    GiveUp ->
      { model | isAnswered <- True }
    NextQuestion ->
      let
        (a, b, s) = newRands model.operator model.seed
      in
        { numA = a
        , numB = b
        , input = ""
        , answer = (solve model.operator) a b
        , seed = s
        , isCorrect = False
        , isAnswered = False
        , operator = model.operator
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
          , span [] [ text <| "The correct answer is" ++ (toString model.answer) ]
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
            [ h2 [] [ text <| toString model.numA ]
            , h2 [] [ text <| toString model.numB ]
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
