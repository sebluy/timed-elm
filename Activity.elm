module Activity where

import Session
import Styles

import Date exposing (Date)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

-- MODEL

type alias Model =
  { name: String
  , sessions: List Session.Model
  }

init: String -> Model
init name =
  { name = name
  , sessions = []
  }

-- UPDATE

type Action = AddSession Session.Model
            | DeleteSession Session.Model
            | StartSession
            | FinishSession Session.Model

update: Action -> Date -> Model -> Model
update action now activity =
  case action of
    AddSession session ->
      { activity | sessions = session :: activity.sessions }

    DeleteSession session ->
      let
        f = not << Session.equal session
      in
        { activity | sessions = List.filter f activity.sessions }

    StartSession ->
      update (AddSession { start = now, finish = Nothing }) now activity

    FinishSession session ->
      let
        f = \otherSession -> if Session.equal session otherSession
                             then
                               { session | finish = Just now }
                             else
                               session
      in
        { activity | sessions = List.map f activity.sessions }

unfinishedSessions: Model -> List Session.Model
unfinishedSessions activity =
  let
    f = \session -> case session.finish of
                      Just _ ->
                        False
                      Nothing ->
                        True
  in
    List.filter f activity.sessions

effectiveDuration: Maybe Time -> Time
effectiveDuration duration =
  case duration of
    Just d ->
      d
    Nothing ->
      0

duration: Model -> Time
duration activity =
  List.foldl (+) 0 (List.map (effectiveDuration << Session.duration) activity.sessions)

durationString : Time -> String
durationString duration =
  let
    hours = floor (Time.inHours duration) % 24
    minutes = floor (Time.inMinutes duration) % 60
    seconds = floor (Time.inSeconds duration) % 60
  in
    List.map (String.padLeft 2 '0' << toString) [hours, minutes, seconds]
      |> String.join ":"

-- VIEW

view : Signal.Address String -> Model -> Html
view address activity =
  tr [ style Styles.tr ]
     [ td [ onClick address activity.name
          , style Styles.tdText
          ]
          [ text activity.name ]
     , td [ style Styles.tdText ]
          [ text (durationString (duration activity)) ]
     ]
