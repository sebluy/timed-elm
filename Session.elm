module Session where

import Date exposing (Date)

-- MODEL

type alias Model =
  { start: Date
  , finish: Date
  }
