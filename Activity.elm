module Activity where

import Session

type alias Model =
  { name: String
  , sessions: List Session.Model
  }

init: String -> Model
init name =
  { name = name
  , sessions = []
  }
