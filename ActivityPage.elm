module ActivityPage where

import Session
import Activity
import SessionForm
import Styles
import Homeless exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model =
  { activityName: String
  , sessionForm: SessionForm.Model
  }

init : String -> Model
init activityName =
  { activityName = activityName
  , sessionForm = SessionForm.init
  }

-- UPDATE

type Action = UpdateSessionForm SessionForm.Action

update : Action -> Model -> Model
update action model =
  case action of
    UpdateSessionForm formAction ->
      { model | sessionForm = SessionForm.update formAction model.sessionForm }

-- VIEW

type alias Context =
  { actions : Signal.Address Action
  , updateActivity: Signal.Address Activity.Action
  , deleteActivity: Signal.Address ()
  , goHome : Signal.Address ()
  }

view : Context -> Date -> Activity.Model -> Model -> Html
view context now activity activityPage =
  div [ style Styles.div ]
     ([ h1 [ style Styles.h1 ]
           [ text activity.name

           , button [ onClick context.goHome ()
                    , style Styles.button
                    ]
                    [ text "Go home" ]

           , case List.head (Activity.unfinishedSessions activity) of
               Nothing ->
                 button [ onClick context.updateActivity (Activity.StartSession)
                        , style Styles.button
                        ]
                        [ text "Start session" ]
               Just session ->
                 button [ style Styles.button
                        , onClick context.updateActivity (Activity.FinishSession session)
                        ]
                        [ text "Finish"]

           , button [ onClick context.actions (UpdateSessionForm SessionForm.Open)
                    , style Styles.button
                    ]
                    [ text "New session" ]

           , button [ onClick context.deleteActivity ()
                    , style Styles.button
                    ]
                    [ text "Delete" ]
            ]
       ]
       ++ [ SessionForm.view (Signal.forwardTo context.actions UpdateSessionForm) activityPage.sessionForm ]
       ++ [ sessionListView context now activity.sessions ]
      )

sessionListView : Context -> Date -> List Session.Model -> Html
sessionListView context now sessions =
  if List.isEmpty sessions
  then
    p [ style Styles.centered ] [ text "No sessions" ]
  else
    table [ style Styles.tableWide ]
          [ tbody []
                  (List.map (sessionView context now) sessions)
          ]

finishTimeString : Maybe Date -> String
finishTimeString finish =
  case finish of
    Just finish ->
      formatDate finish
    Nothing ->
      "Unfinished"

sessionView : Context -> Date -> Session.Model -> Html
sessionView context now session =
  tr [ style Styles.tr ]
     [ td [ style Styles.td ]
          [ text <| formatDate <| session.start ]
     , td [ style Styles.td ]
          [ text <| finishTimeString session.finish ]
     , td [ style Styles.td ]
          [ text <| durationString <| Session.duration now session ]
     , td [ style Styles.td ]
          [ button [ style Styles.button
                   , onClick context.updateActivity (Activity.DeleteSession session)
                   ]
                   [ text "Delete" ]
          ]
     ]
