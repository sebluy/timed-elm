module Session where

import Date exposing (Date)
import Time exposing (Time)

-- MODEL

type alias Model =
  { start: Date
  , finish: Maybe Date
  }

type alias Store =
  { start: Time
  , finish: (String, Time)
  }

store : Model -> Store
store session =
  { start = Date.toTime session.start
  , finish = case session.finish of
               Just finish ->
                 ("Just", Date.toTime finish)
               Nothing ->
                 ("Nothing", 0)
  }

unstore : Store -> Model
unstore store =
  { start = Date.fromTime store.start
  , finish = if (fst store.finish) == "Just"
             then
               Just (Date.fromTime (snd store.finish))
             else
               Nothing
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
