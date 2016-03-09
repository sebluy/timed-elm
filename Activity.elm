module Activity where

import Session
import Styles
import Homeless exposing (..)

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

duration: Date -> Model -> Time
duration now activity =
  List.map (Session.duration now) activity.sessions
    |> List.foldl (+) 0

durationToday: Date -> Model -> Time
durationToday now activity =
  List.filter (isToday now << .start) activity.sessions
    |> List.map (Session.duration now)
    |> List.foldl (+) 0

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
                               otherSession
      in
        { activity | sessions = List.map f activity.sessions }

-- VIEW

view : Signal.Address String -> Date -> Model -> Html
view address now activity =
  tr [ style Styles.tr ]
     [ td [ onClick address activity.name
          , style Styles.tdText
          ]
          [ text activity.name ]
     , td [ style Styles.tdText ]
          [ text (durationString (durationToday now activity)) ]
     ]
