module ActivityList where

import Session
import Activity
import SessionForm
import ActivityPage
import Styles
import Homeless exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model = List Activity.Model

find : String -> Model -> Maybe Activity.Model
find name activities =
  List.filter ((==) name << .name) activities
    |> List.head

-- UPDATE

type Action = Create Activity.Model
            | Delete Activity.Model
            | Update Activity.Model Activity.Action

update : Action -> Date -> Model -> Model
update action now activities =
  case action of
    Create activity ->
      if List.member activity activities
      then
        activities
      else
        activity :: activities

    Delete activity ->
      List.filter (\someActivity -> activity.name /= someActivity.name) activities

    Update activity activityAction ->
      let
        f = \someActivity -> if someActivity.name == activity.name
                             then
                               Activity.update activityAction now someActivity
                             else
                               someActivity
      in
        List.map f activities

-- VIEW

view : Signal.Address String -> Model -> Html
view address activities =
  if List.isEmpty activities
  then
    p [ style Styles.centered ] [ text "No activities" ]
  else
    table [ style Styles.tableNarrow ]
          [ tbody []
                  (List.map (Activity.view address) activities)
          ]
