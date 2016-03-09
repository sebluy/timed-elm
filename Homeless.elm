module Homeless where

import Date exposing (Date)
import Time exposing (Time)
import String
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


onInput : (String -> Signal.Message) -> Attribute
onInput f =
  on "input" targetValue f


onEnter : Signal.Address a -> a -> a -> Attribute
onEnter address good bad =
  let
    enterKey = 13
  in
    onKeyUp address (\key -> if (enterKey == key)
                             then good
                             else bad)

formatDate : Date -> String
formatDate date =
  Date.Format.format "%B %e, %Y %l:%M %p" date

strDateStr : String -> String
strDateStr str =
  case Date.fromString str of
    Ok date ->
      formatDate date
    Err error ->
      error

durationString : Time -> String
durationString duration =
  let
    hours = floor (Time.inHours duration) % 24
    minutes = floor (Time.inMinutes duration) % 60
    seconds = floor (Time.inSeconds duration) % 60
  in
    List.map (String.padLeft 2 '0' << toString) [hours, minutes, seconds]
      |> String.join ":"

midnight : Date -> Date
midnight date =
  let
    hour = toFloat (Date.hour date)
    minute = toFloat (Date.minute date)
    second = toFloat (Date.second date)
    millisecond = toFloat (Date.millisecond date)
  in
    Date.toTime date
      - hour * Time.hour
      - minute * Time.minute
      - second * Time.second
      - millisecond * Time.millisecond
      |> Date.fromTime


isToday : Date -> Date -> Bool
isToday now date =
  let
    dateTime = Date.toTime date
    startDay = Date.toTime (midnight now)
  in
     dateTime >= startDay && dateTime < startDay + 24 * Time.hour
