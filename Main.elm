import StartApp.Simple
import Session
import SessionForm
import Styles
import Homeless exposing (..)
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
  { activities: List Activity
  , createInput: String
  , page: Page
  }

type Page = HomePage |
            ActivityPage ActivityPageData

type alias ActivityPageData =
  { activityName: String
  , sessionForm: SessionForm.Model
  }

type alias Activity =
  { name: String
  , sessions: List Session.Model
  }

init : Model
init = { activities = [ initActivity "Init" ]
       , createInput = ""
       , page = HomePage
       }

initActivityPage : String -> ActivityPageData
initActivityPage name =
  { activityName = name
  , sessionForm = SessionForm.init
  }

initActivity : String -> Activity
initActivity name = { name = name
                    , sessions = []
                    }

-- UPDATE

type Action = Create
            | Delete String
            | UpdateCreateInput String
            | SetPage Page
            | UpdateSessionForm SessionForm.Action
            | Nop

update : Action -> Model -> Model
update action model =
  case action of

    Create ->
      let name = model.createInput
          maybeActivity = findActivity name model.activities
      in { model | activities = case maybeActivity of
                                  Just a -> model.activities
                                  Nothing -> (initActivity name) :: model.activities
                 , createInput = ""
         }

    Delete activityName ->
      { model |
        activities = model.activities
                     |> List.filter (\activity -> activity.name /= activityName)
      , page = HomePage
      }

    UpdateCreateInput str ->
      { model | createInput = str }

    SetPage page ->
      { model | page = page }

    UpdateSessionForm subAction ->
      case model.page of
        ActivityPage pageData ->
          { model
            | page = ActivityPage { pageData
                                    | sessionForm = SessionForm.update subAction pageData.sessionForm
                                  }
            , activities = case subAction of
                             SessionForm.Submit ->
                               let
                                 maybeSession = SessionForm.parseSession pageData.sessionForm
                               in
                                 maybeAddSession maybeSession pageData.activityName model.activities
                             _ ->
                               model.activities

          }
        _ ->
          model

    Nop ->
      model


addSessionToActivity : Session.Model -> Activity -> Activity
addSessionToActivity session activity =
  { activity
    | sessions = session :: activity.sessions
  }

maybeAddSession : Maybe Session.Model -> String -> List Activity -> List Activity
maybeAddSession maybeSession name activities =
  case maybeSession of
    Just session ->
      addSession session name activities
    Nothing ->
      activities

addSession : Session.Model -> String -> List Activity -> List Activity
addSession session name activities =
  List.map (\activity -> if activity.name == name
                         then
                           addSessionToActivity session activity
                         else
                           activity)
           activities


findActivity : String -> List Activity -> Maybe Activity
findActivity name activities =
  activities
    |> List.filter (\activity -> activity.name == name)
    |> List.head


-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  case model.page of
    HomePage ->
      homePageView address model
    ActivityPage activityPage ->
      let maybeActivity = model.activities
                          |> List.filter (\activity -> activity.name == activityPage.activityName)
                          |> List.head
      in
        case maybeActivity of
          Just activity ->
            activityPageView address activity activityPage
          Nothing ->
            notFoundPageView address

notFoundPageView : Signal.Address Action -> Html
notFoundPageView address =
  div [ style Styles.div ] [ text "Page not found..." ]

homePageView : Signal.Address Action -> Model -> Html
homePageView address model =
  let message f value = Signal.message address (f value)
      createInput = input [ placeholder "Create activity..."
                          , value model.createInput
                          , onInput (message UpdateCreateInput)
                          , onEnter address Create Nop
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


activityPageView : Signal.Address Action -> Activity -> ActivityPageData -> Html
activityPageView address activity activityPage =
  div [ style Styles.div ]
     ([ h1 [ style Styles.h1 ]
           [ text activity.name
           , button [ onClick address (SetPage HomePage)
                    , style Styles.button
                    ]
                    [ text "Go back" ]
           , button [ onClick address (UpdateSessionForm SessionForm.Open)
                    , style Styles.button
                    ]
                    [ text "New session" ]

           , button [ onClick address (Delete activity.name)
                    , style Styles.button
                    ]
                    [ text "Delete" ]
            ]
       ]
       ++ [ SessionForm.view (Signal.forwardTo address UpdateSessionForm) activityPage.sessionForm ]
       ++ [ sessionListView address activity.sessions ]
      )

sessionListView : Signal.Address Action -> List Session.Model -> Html
sessionListView address sessions =
  if List.isEmpty sessions
  then
    p [ style Styles.centered ] [ text "No sessions" ]
  else
    table [ style Styles.table ]
          [ tbody []
                  (List.map (sessionView address) sessions)
          ]


sessionView : Signal.Address Action -> Session.Model -> Html
sessionView address session =
  tr [ style Styles.tr ]
     [ td [ style Styles.tdText ]
          [ text (formatDate session.start) ]
     , td [ style Styles.tdText ]
          [ text (formatDate session.finish) ]
     ]


activityListView : Signal.Address Action -> List Activity -> Html
activityListView address activities =
  if List.isEmpty activities
  then
    p [ style Styles.centered ] [ text "No activities" ]
  else
    table [ style Styles.table ]
          [ tbody []
                  (List.map (activityView address) activities)
          ]


activityView : Signal.Address Action -> Activity -> Html
activityView address activity =
  tr [ style Styles.tr ]
     [ td [ onClick address (SetPage (ActivityPage (initActivityPage activity.name)))
          , style Styles.tdText
          ]
          [ text activity.name ]
     ]
