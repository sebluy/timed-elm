import StartApp.Simple

import Session
import Activity
import SessionForm
import ActivityPage
import Styles
import Homeless exposing (..)

import Date exposing (Date)
import Time
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }

-- MODEL

type alias Model =
  { activities: List Activity.Model
  , createInput: String
  , page: Page
  }

type Page = HomePage |
            ActivityPage ActivityPage.Model

init : Model
init = { activities = [ { name = "Init"
                        , sessions = [ { start = Date.fromTime 1457401963788
                                       , finish = Date.fromTime 1457402044358
                                       }
                                     ]
                        }
                      ]
       , createInput = ""
       , page = HomePage
       }


-- UPDATE

type Action = CreateActivity
            | DeleteActivity String
            | DeleteSession String Date
            | AddSession Session.Model String
            | UpdateCreateInput String
            | SetPage Page
            | UpdateActivityPage ActivityPage.Action
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

activityPageContext : Signal.Address Action -> ActivityPage.Context
activityPageContext address =
  { actions = Signal.forwardTo address UpdateActivityPage
  , deleteActivity = Signal.forwardTo address DeleteActivity
  , deleteSession = Signal.forwardTo address (\(name, start) -> DeleteSession name start)
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
            ActivityPage.view (activityPageContext address) activity activityPage
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
    table [ style Styles.table ]
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
          [ text (Activity.duration activity
                  |> Time.inHours
                  |> toString
                  |> String.left 3
                  |> (\duration -> duration ++ " hours"))
          ]
     ]
