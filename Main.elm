import StartApp

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
import Effects

app =
  StartApp.start
    { init = (init, Effects.none)
    , update = \action model -> (update action model, Effects.none)
    , view = view
    , inputs = [ Signal.map
                   (\time -> UpdateNow (Date.fromTime time))
                   (Time.every Time.second)
               ]
    }

main = app.html

-- MODEL

type alias Model =
  { activities: List Activity.Model
  , createInput: String
  , page: Page
  , now: Date
  }

type Page = HomePage |
            ActivityPage ActivityPage.Model

init : Model
init = { activities = [ { name = "Init"
                        , sessions = [ { start = Date.fromTime 1457401963788
                                       , finish = Just (Date.fromTime 1457402044358)
                                       }
                                     ]
                        }
                      ]
       , createInput = ""
       , page = HomePage
       , now = Date.fromTime 1457469219249
       }


-- UPDATE

type Action = CreateActivity
            | DeleteActivity String
            | DeleteSession String Date
            | StartSession String
            | FinishSession String Date
            | AddSession Session.Model String
            | UpdateCreateInput String
            | SetPage Page
            | UpdateActivityPage ActivityPage.Action
            | UpdateNow Date
            | Nop

update : Action -> Model -> Model
update action model =
  case action of

    CreateActivity ->
      let name = model.createInput
          maybeActivity = findActivity name model.activities
      in { model | activities = case maybeActivity of
                                  Just a -> model.activities
                                  Nothing -> (Activity.init name) :: model.activities
                 , createInput = ""
         }

    DeleteActivity activityName ->
      { model
        | activities = model.activities
                       |> List.filter (\activity -> activity.name /= activityName)
        , page = HomePage
      }

    DeleteSession activityName startDate ->
      { model
        | activities = model.activities
                      |> List.map (\activity -> if activityName == activity.name
                                                then
                                                  Activity.deleteSession startDate activity
                                                else
                                                  activity)
      }

    StartSession activityName ->
      { model
        | activities = model.activities
                      |> List.map (\activity -> if activityName == activity.name
                                                then
                                                  Activity.startSession model.now activity
                                                else
                                                  activity)
      }

    FinishSession activityName startDate ->
      { model
        | activities = model.activities
                      |> List.map (\activity -> if activityName == activity.name
                                                then
                                                  Activity.finishSession startDate model.now activity
                                                else
                                                  activity)
      }

    AddSession session activityName ->
      { model | activities = addSession session activityName model.activities }

    UpdateCreateInput str ->
      { model | createInput = str }

    SetPage page ->
      { model | page = page }

    UpdateActivityPage pageAction ->
      case model.page of
        ActivityPage page ->
          { model | page = ActivityPage (ActivityPage.update pageAction page) }
          |> handleActivityPageAction pageAction page
        _ ->
          model

    UpdateNow now ->
      { model | now = now }

    Nop ->
      model

handleActivityPageAction : ActivityPage.Action -> ActivityPage.Model -> Model -> Model
handleActivityPageAction action page model =
  case action of
    ActivityPage.UpdateSessionForm formAction ->
      case formAction of
        SessionForm.Submit ->
          let
            maybeSession = SessionForm.parseSession page.sessionForm
          in
            case maybeSession of
              Just session ->
                update (AddSession session page.activityName) model
              Nothing ->
                model
        _ ->
          model

addSession : Session.Model -> String -> List Activity.Model -> List Activity.Model
addSession session name activities =
  List.map (\activity -> if activity.name == name
                         then
                           Activity.addSession session activity
                         else
                           activity)
           activities


findActivity : String -> List Activity.Model -> Maybe Activity.Model
findActivity name activities =
  activities
    |> List.filter (\activity -> activity.name == name)
    |> List.head

-- VIEW

activityPageContext : Signal.Address Action -> Activity.Model -> ActivityPage.Context
activityPageContext address activity =
  { actions = Signal.forwardTo address UpdateActivityPage
  , deleteActivity = Signal.forwardTo address DeleteActivity
  , deleteSession = Signal.forwardTo address (DeleteSession activity.name)
  , startSession = Signal.forwardTo address (always (StartSession activity.name))
  , finishSession = Signal.forwardTo address (FinishSession activity.name)
  , goHome = Signal.forwardTo address (always (SetPage HomePage))
  }

view : Signal.Address Action -> Model -> Html
view address model =
  case model.page of
    HomePage ->
      homePageView address model
    ActivityPage activityPage ->
      let
        maybeActivity = findActivity activityPage.activityName model.activities
      in
        case maybeActivity of
          Just activity ->
            ActivityPage.view (activityPageContext address activity) activity activityPage
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
                          , onEnter address CreateActivity Nop
                          , style Styles.centeredInput
                          ]
                          []
  in div [] ([ div
                 [ style Styles.div ]
                 [ createInput ]
             , div
                 [ style Styles.div ]
                 [ (activityListView address model.activities) ]
             ])

activityListView : Signal.Address Action -> List Activity.Model -> Html
activityListView address activities =
  if List.isEmpty activities
  then
    p [ style Styles.centered ] [ text "No activities" ]
  else
    table [ style Styles.tableNarrow ]
          [ tbody []
                  (List.map (activityView address) activities)
          ]

activityView : Signal.Address Action -> Activity.Model -> Html
activityView address activity =
  tr [ style Styles.tr ]
     [ td [ onClick address (SetPage (ActivityPage (ActivityPage.init activity.name)))
          , style Styles.tdText
          ]
          [ text activity.name ]
     , td [ style Styles.tdText ]
          [ text (durationString (Activity.duration activity)) ]
     ]

durationString : Time -> String
durationString duration =
  let
    hours = floor (Time.inHours duration) % 24
    minutes = floor (Time.inMinutes duration) % 60
    seconds = floor (Time.inSeconds duration) % 60
  in
    List.map (String.padLeft 2 '0' << toString) [hours, minutes, seconds]
      |> String.join ":"
