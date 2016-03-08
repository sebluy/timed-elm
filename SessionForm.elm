module SessionForm where

import Session
import Styles
import Homeless exposing (..)
import Date exposing (Date)
import Time exposing (Time)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model = Maybe SessionForm

type alias SessionForm =
  { start: String
  , finish: String
  }

init : Model
init = Nothing

-- UPDATE

type Action = Open
            | Close
            | UpdateStartInput String
            | UpdateFinishInput String
            | Submit
            | Nop

update: Action -> Model -> Model
update action model =
  case action of

    Open ->
      Just { start = "", finish = "" }

    Close ->
      Nothing

    UpdateStartInput str ->
      case model of
        Just form ->
          Just { form | start = str }
        Nothing ->
          Nothing

    UpdateFinishInput str ->
      case model of
        Just form ->
          Just { form | finish = str }
        Nothing ->
          Nothing

    Submit ->
      update Close model

    Nop ->
      model


parseSession : Model -> Maybe Session.Model
parseSession model =
  case model of
    Just form ->
      case Date.fromString form.start of
        Ok startDate ->
          if form.finish == ""
          then
            Just { start = startDate, finish = Nothing }
          else
            case Date.fromString form.finish of
              Ok finishDate ->
                Just { start = startDate, finish = Just finishDate }
              _ ->
                Nothing
        _ ->
          Nothing
    Nothing ->
      Nothing

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model of
    Just form ->
      let
        message f val = Signal.message address (f val)
      in
        div [] [ input [ placeholder "Start..."
                       , value form.start
                       , onInput (message (\str -> (UpdateStartInput str)))
                       , onEnter address Submit Nop
                       , style Styles.input
                       ]
                       []
               , strDateView form.start
               , input [ placeholder "Finish..."
                       , value form.finish
                       , onInput (message (\str -> (UpdateFinishInput str)))
                       , onEnter address Submit Nop
                       , style Styles.input
                       ]
                       []
               , strDateView form.finish
               ]
    Nothing ->
      div [] []


strDateView : String -> Html
strDateView dateStr =
  p [] [ text (case (strDateStr dateStr) of
                 Ok str -> str
                 Err str -> str)
       ]
