module Session where

import Date exposing (Date)
import Time

-- MODEL

type alias Model =
  { start: Date
  , finish: Date
  }

duration : Model -> Time.Time
duration session =
  (Date.toTime session.finish) - (Date.toTime session.start)
