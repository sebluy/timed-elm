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
startSession startDate activity =
  addSession { start = startDate, finish = Nothing } activity

deleteSession: Date -> Model -> Model
deleteSession startDate activity =
  { activity
    | sessions =
        List.filter
          (\session -> Date.toTime session.start /= Date.toTime startDate)
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
