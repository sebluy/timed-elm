module Homeless where

import Date exposing (Date)
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
