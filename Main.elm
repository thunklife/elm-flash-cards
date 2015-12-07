module Main where

import Maybe exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import FlashCard as FC

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
      div [ class "container" ]
          [ div [ class "choose-operator"]
                (List.map (operatorButton address) model.operators)
          ]
    (Just flashCard) ->
      div [ class "container" ]
          [ FC.view (Signal.forwardTo address Modify) flashCard
          , resetButton address
          ]


operatorButton : Signal.Address Action -> FC.Operator -> Html
operatorButton address op =
  button [ onClick address <| Select op]
         [ text <| FC.operator op ]

resetButton : Signal.Address Action -> Html
resetButton address =
  div [ class "reset-button" ]
      [ button [ onClick address Reset] [ text "Start Over" ] ]

main : Signal Html
main = StartApp.start { view = view, update = update, model = initialModel }
