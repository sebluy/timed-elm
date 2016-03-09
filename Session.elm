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

duration : Model -> Maybe Time
duration session =
  case session.finish of
    Just finish ->
      Just (Date.toTime finish - Date.toTime session.start)
    Nothing ->
      Nothing
