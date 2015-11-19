module Main where

import Maybe exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import FlashCard as FC
import String as S

import StartApp.Simple as StartApp

--MODEL

type alias Model =
  { flashCard : Maybe FC.Model
  , operators : List FC.Operator
  }

initialModel : Model
initialModel =
  { flashCard = Nothing
  , operators = [ FC.Add, FC.Sub, FC.Mul, FC.Div ]
  }

-- UPDATE

type Action
  = Select FC.Operator
  | Modify FC.Action
  | Reset

update : Action -> Model -> Model
update action model =
  case action of
    (Select op) ->
      { model | flashCard <- Just <| FC.init op}
    Modify cardAction ->
      case model.flashCard of
        Nothing ->
          model
        Just flashCard ->
          { model | flashCard <- Just <| FC.update cardAction flashCard }
    Reset ->
      initialModel

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.flashCard of
    Nothing ->
      div []
          [ div [ class "choose-operator"]
                (List.map (operatorButton address) model.operators)
          , resetButton address
          ]
    (Just flashCard) ->
      div []
          [ FC.view (Signal.forwardTo address Modify) flashCard
          , resetButton address
          ]


operatorButton : Signal.Address Action -> FC.Operator -> Html
operatorButton address op =
  button [ class <| operatorText op
         , onClick address <| Select op]
         [ text <| operatorText op ]

resetButton : Signal.Address Action -> Html
resetButton address =
  button [ class "reset"
         , onClick address Reset
         ]
         [ text "Start Over" ]

operatorText : FC.Operator -> String
operatorText = S.toLower << toString

main : Signal Html
main = StartApp.start { view = view, update = update, model = initialModel }
