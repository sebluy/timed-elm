module Session where

import Date exposing (Date)
import Time exposing (Time)

-- MODEL

type alias Model =
  { start: Date
  , finish: Maybe Date
  }

equal : Model -> Model -> Bool
equal s1 s2 =
  Date.toTime s1.start == Date.toTime s2.start

duration : Date -> Model -> Time
duration now session =
  case session.finish of
    Just finish ->
      Date.toTime finish - Date.toTime session.start
    Nothing ->
      Date.toTime now - Date.toTime session.start
