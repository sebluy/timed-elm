module Activity where

import Session
import Time
import Date exposing (Date)

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

deleteSession: Date -> Model -> Model
deleteSession startDate activity =
  { activity
    | sessions =
        List.filter
          (\session -> (Date.toTime session.start) /= (Date.toTime startDate))
          activity.sessions
  }

duration: Model -> Time.Time
duration activity =
  List.foldl (+) 0 (List.map Session.duration activity.sessions)
