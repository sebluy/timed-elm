module Activity where

import Session
import Date exposing (Date)
import Time exposing (Time)

type alias Model =
  { name: String
  , sessions: List Session.Model
  }

init: String -> Model
init name =
  { name = name
  , sessions = []
  }

addSession: Session.Model -> Model -> Model
addSession session activity =
  { activity | sessions = session :: activity.sessions }

startSession: Date -> Model -> Model
startSession start activity =
  addSession { start = start , finish = Nothing } activity

finishSession: Date -> Date -> Model -> Model
finishSession start finish activity =
  { activity
    | sessions = List.map (\session -> if Date.toTime session.start == Date.toTime start
                                       then
                                         { start = start, finish = Just finish }
                                       else
                                         session)
                          activity.sessions
  }

unfinishedSessions: Model -> List Session.Model
unfinishedSessions activity =
  List.filter (\session -> case session.finish of
                             Just _ ->
                               False
                             Nothing ->
                               True)
              activity.sessions

deleteSession: Date -> Model -> Model
deleteSession start activity =
  { activity
    | sessions =
        List.filter
          (\session -> Date.toTime session.start /= Date.toTime start)
          activity.sessions
  }

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
