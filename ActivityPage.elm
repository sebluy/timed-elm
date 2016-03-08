module ActivityPage where

import Session
import Activity
import SessionForm
import Styles
import Homeless exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model =
  { activityName: String
  , sessionForm: SessionForm.Model
  }

init : String -> Model
init name =
  { activityName = name
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
  , delete : Signal.Address String
  , goHome : Signal.Address ()
  }

view : Context -> Activity.Model -> Model -> Html
view context activity activityPage =
  div [ style Styles.div ]
     ([ h1 [ style Styles.h1 ]
           [ text activity.name
           , button [ onClick context.goHome ()
                    , style Styles.button
                    ]
                    [ text "Go home" ]
           , button [ onClick context.actions (UpdateSessionForm SessionForm.Open)
                    , style Styles.button
                    ]
                    [ text "New session" ]

           , button [ onClick context.delete activity.name
                    , style Styles.button
                    ]
                    [ text "Delete" ]
            ]
       ]
       ++ [ SessionForm.view (Signal.forwardTo context.actions UpdateSessionForm) activityPage.sessionForm ]
       ++ [ sessionListView activity.sessions ]
      )

sessionListView : List Session.Model -> Html
sessionListView sessions =
  if List.isEmpty sessions
  then
    p [ style Styles.centered ] [ text "No sessions" ]
  else
    table [ style Styles.table ]
          [ tbody []
                  (List.map sessionView sessions)
          ]


sessionView : Session.Model -> Html
sessionView session =
  tr [ style Styles.tr ]
     [ td [ style Styles.tdText ]
          [ text (formatDate session.start) ]
     , td [ style Styles.tdText ]
          [ text (formatDate session.finish) ]
     ]