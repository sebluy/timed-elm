import StartApp

import Session
import Activity
import ActivityList
import SessionForm
import ActivityPage
import Styles
import Homeless exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import String
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects

main : Signal Html
main =
  Signal.map (view actions.address) model

model : Signal Model
model =
  Signal.foldp update initialModel (Signal.merge actions.signal tick)

initialModel : Model
initialModel =
  init (ActivityList.unstore (Maybe.withDefault [] getActivities))

actions : Signal.Mailbox Action
actions =
  Signal.mailbox Nop

tick : Signal Action
tick =
  Signal.map
    (\time -> UpdateNow (Date.fromTime time))
    (Time.every (100 * Time.millisecond))

port setActivities : Signal ActivityList.Store
port setActivities =
  Signal.dropRepeats (Signal.map (ActivityList.store << .activities) model)

port getActivities : Maybe ActivityList.Store

-- MODEL

type alias Model =
  { activities: ActivityList.Model
  , createInput: String
  , page: Page
  , now: Date
  }

type Page = HomePage |
            ActivityPage ActivityPage.Model

init : ActivityList.Model -> Model
init activities =
  { activities = activities
  , createInput = ""
  , page = HomePage
  , now = Date.fromTime 0
  }

-- UPDATE

type Action = UpdateActivities ActivityList.Action
            | UpdateCreateInput String
            | SubmitCreateInput
            | SetPage Page
            | UpdateActivityPage ActivityPage.Action
            | UpdateNow Date
            | Nop

update : Action -> Model -> Model
update action model =
  case action of
    UpdateActivities activitiesAction ->
      { model | activities = ActivityList.update activitiesAction model.now model.activities }
        |> interceptActivitiesAction activitiesAction

    SubmitCreateInput ->
      update (UpdateActivities (ActivityList.Create (Activity.init model.createInput))) model
        |> update (UpdateCreateInput "")

    UpdateCreateInput str ->
      { model | createInput = str }

    SetPage page ->
      { model | page = page }

    UpdateActivityPage pageAction ->
      case model.page of
        ActivityPage page ->
          { model | page = ActivityPage (ActivityPage.update pageAction page) }
            |> interceptActivityPageAction pageAction page
        _ ->
          model

    UpdateNow now ->
      { model | now = now }

    Nop ->
      model

interceptActivitiesAction : ActivityList.Action -> Model -> Model
interceptActivitiesAction action model =
  case action of
    ActivityList.Delete _ ->
      { model | page = HomePage }
    _ ->
      model

interceptActivityPageAction : ActivityPage.Action -> ActivityPage.Model -> Model -> Model
interceptActivityPageAction action page model =
  case action of
    ActivityPage.UpdateSessionForm formAction ->
      case (formAction,
            SessionForm.parseSession page.sessionForm,
            ActivityList.find page.activityName model.activities) of
        (SessionForm.Submit, Just session, Just activity) ->
          update (UpdateActivities
                    (ActivityList.Update activity
                       (Activity.AddSession session)))
                 model
        _ ->
          model

-- VIEW

activityPageContext : Signal.Address Action -> Activity.Model -> ActivityPage.Context
activityPageContext address activity =
  { actions = Signal.forwardTo address UpdateActivityPage
  , deleteActivity = Signal.forwardTo address (always (UpdateActivities (ActivityList.Delete activity)))
  , updateActivity = Signal.forwardTo address (UpdateActivities << ActivityList.Update activity)
  , goHome = Signal.forwardTo address (always (SetPage HomePage))
  }

view : Signal.Address Action -> Model -> Html
view address model =
  case model.page of
    HomePage ->
      homePageView address model
    ActivityPage page ->
      case ActivityList.find page.activityName model.activities of
        Just activity ->
          ActivityPage.view (activityPageContext address activity) model.now activity page
        Nothing ->
          notFoundPageView

notFoundPageView : Html
notFoundPageView =
  div [ style Styles.div ] [ text "Page not found..." ]

homePageView : Signal.Address Action -> Model -> Html
homePageView address model =
  let message f value = Signal.message address (f value)
      createInput = input [ placeholder "Create activity..."
                          , value model.createInput
                          , onInput (message UpdateCreateInput)
                          , onEnter address SubmitCreateInput Nop
                          , style Styles.centeredInput
                          ]
                          []
      activitiesAddress = Signal.forwardTo address (SetPage << ActivityPage << ActivityPage.init)
  in
    div [] [ div
               [ style Styles.div ]
               [ createInput ]
           , div
               [ style Styles.div ]
               [ ActivityList.view activitiesAddress model.now model.activities ]
           ]
