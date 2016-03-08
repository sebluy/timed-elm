module Session where

import Date exposing (Date)
import Time exposing (Time)

-- MODEL

type alias Model =
  { start: Date
  , finish: Maybe Date
  }

duration : Model -> Maybe Time
duration session =
  case session.finish of
    Just finish ->
      Just (Date.toTime finish - Date.toTime session.start)
    Nothing ->
      Nothing
