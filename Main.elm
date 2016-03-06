import StartApp.Simple
import Date exposing (Date)
import Time exposing (Time)
import Date.Format
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
  , sessionForm: Maybe SessionForm
  }


type alias SessionForm =
  { start: String
  , finish: String
  }

type alias Activity =
  { name: String
  , sessions: List Session
  }

type alias Session =
  { start: Date
  , finish: Date
  }

init : Model
init = { activities = [ initActivity "Init" ]
       , createInput = ""
       , page = HomePage
       }

initActivityPage : String -> ActivityPageData
initActivityPage name =
  { activityName = name
  , sessionForm = Nothing
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
            | OpenSessionForm
            | UpdateSessionFormStartInput String
            | UpdateSessionFormFinishInput String
            | SubmitSessionForm
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

    OpenSessionForm ->
      case model.page of
        ActivityPage pageData ->
          { model | page = ActivityPage { pageData | sessionForm = Just { start = ""
                                                                        , finish = ""
                                                                        }
                                        }
          }
        _ ->
          model

    UpdateSessionFormStartInput str ->
      case model.page of
        ActivityPage pageData ->
          let
            sessionForm = pageData.sessionForm
          in
            { model
              | page = ActivityPage { pageData
                                      | sessionForm = case sessionForm of
                                                        Just form ->
                                                          Just { form
                                                                 | start = str
                                                               }
                                                        Nothing ->
                                                          Nothing
                                    }
            }
        _ ->
          model

    UpdateSessionFormFinishInput str ->
      case model.page of
        ActivityPage pageData ->
          let
            sessionForm = pageData.sessionForm
          in
            { model
              | page = ActivityPage { pageData
                                      | sessionForm = case sessionForm of
                                                        Just form ->
                                                          Just { form
                                                                 | finish = str
                                                               }
                                                        Nothing ->
                                                          Nothing
                                    }
            }
        _ ->
          model

    SubmitSessionForm ->
      case model.page of
        ActivityPage pageData ->
          case pageData.sessionForm of
            Just form ->
              case (Date.fromString form.start, Date.fromString form.finish) of
                (Ok start, Ok finish) ->
                  let
                    session = { start = start, finish = finish }
                  in
                    { model
                      | page = ActivityPage { pageData | sessionForm = Nothing }
                      , activities = addSession session pageData.activityName model.activities
                    }
                _ -> model
            _ -> model
        _ -> model

    Nop ->
      model

addSessionToActivity : Session -> Activity -> Activity
addSessionToActivity session activity =
  { activity
    | sessions = session :: activity.sessions
  }

addSession : Session -> String -> List Activity -> List Activity
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
  div [ style divStyle ] [ text "Page not found..." ]

homePageView : Signal.Address Action -> Model -> Html
homePageView address model =
  let message f value = Signal.message address (f value)
      createInput = input [ placeholder "Create activity..."
                          , value model.createInput
                          , onInput (message UpdateCreateInput)
                          , onEnter address Create Nop
                          , style centeredInputStyle
                          ]
                          []
  in div [] ([ div
                 [ style divStyle ]
                 [ createInput ]
             , div
                 [ style divStyle ]
                 [ (activityListView address model.activities) ]
             ])


activityPageView : Signal.Address Action -> Activity -> ActivityPageData -> Html
activityPageView address activity activityPage =
  div [ style divStyle ]
     ([ h1 [ style h1Style ]
           [ text activity.name
           , button [ onClick address (SetPage HomePage)
                    , style buttonStyle
                    ]
                    [ text "Go back" ]
           , button [ onClick address OpenSessionForm
                    , style buttonStyle
                    ]
                    [ text "New session" ]

           , button [ onClick address (Delete activity.name)
                    , style buttonStyle
                    ]
                    [ text "Delete" ]
            ]
       ] ++
       (case activityPage.sessionForm of
              Just sessionForm ->
                [ sessionFormView address sessionForm ]
              Nothing ->
                [])
       ++ [ sessionListView address activity.sessions ]
      )

sessionFormView : Signal.Address Action -> SessionForm -> Html
sessionFormView address form =
  let
    message f val = Signal.message address (f val)
  in
    div [] [ input [ placeholder "Start..."
                   , value form.start
                   , onInput (message UpdateSessionFormStartInput)
                   , onEnter address SubmitSessionForm Nop
                   , style inputStyle
                   ]
                   []
           , strDateView form.start
           , input [ placeholder "Finish..."
                   , value form.finish
                   , onInput (message UpdateSessionFormFinishInput)
                   , onEnter address SubmitSessionForm Nop
                   , style inputStyle
                   ]
                   []
           , strDateView form.finish
         ]


strDateView : String -> Html
strDateView dateStr =
  p [] [ text (case (strDateStr dateStr) of
                 Ok str -> str
                 Err str -> str)
       ]

sessionListView : Signal.Address Action -> List Session -> Html
sessionListView address sessions =
  if List.isEmpty sessions
  then
    p [ style centeredStyle ] [ text "No sessions" ]
  else
    table [ style tableStyle ]
          [ tbody []
                  (List.map (sessionView address) sessions)
          ]


sessionView : Signal.Address Action -> Session -> Html
sessionView address session =
  tr [ style trStyle ]
     [ td [ style tdTextStyle ]
          [ text (formatDate session.start) ]
     , td [ style tdTextStyle ]
          [ text (formatDate session.finish) ]
     ]


activityListView : Signal.Address Action -> List Activity -> Html
activityListView address activities =
  if List.isEmpty activities
  then
    p [ style centeredStyle ] [ text "No activities" ]
  else
    table [ style tableStyle ]
          [ tbody []
                  (List.map (activityView address) activities)
          ]


activityView : Signal.Address Action -> Activity -> Html
activityView address activity =
  tr [ style trStyle ]
     [ td [ onClick address (SetPage (ActivityPage (initActivityPage activity.name)))
          , style tdTextStyle
          ]
          [ text activity.name ]
     ]


onInput : (String -> Signal.Message) -> Attribute
onInput f =
  on "input" targetValue f


onEnter : Signal.Address a -> a -> a -> Attribute
onEnter address good bad =
  onKeyUp address (\key -> if (enterKey == key)
                           then good
                           else bad)

enterKey = 13

formatDate : Date -> String
formatDate date =
  Date.Format.format "%Y %B %e %l:%M %p" date

strDateStr : String -> Result String String
strDateStr str =
  case Date.fromString str of
    Ok date ->
      Ok (formatDate date)
    Err error ->
      Err "Bad Date"


-- STYLES


dateFieldStyle : Result String String -> List (String, String)
dateFieldStyle result =
  let style = case result of
                Ok _ ->
                  goodDateStyle
                Err _ ->
                  badDateStyle
  in style ++ inputStyleBorderless


inputStyle =
  [ ("border", "0px") ]
  ++ inputStyleBorderless

centeredInputStyle =
  inputStyle ++ centeredStyle ++
  [ ("width", "100%") ]


inputStyleBorderless =
    [ ("outline", "none")
    , ("font-family", "Arial sans-serif")
    , ("font-size", "16px")
    ]


green = "#00FF00"


red = "#FF0000"


bottomBorderStyle color =
  [ ("border-bottom", "solid 3px " ++ color)
  , ("border-top", "none")
  , ("border-right", "none")
  , ("border-left", "none")
  ]


goodDateStyle = bottomBorderStyle green
badDateStyle = bottomBorderStyle red


tableStyle =
  [ ("width", "50%")
  , ("margin", "auto")
  ]

tdTextStyle =
  ("font-size", "30px") :: tdStyle

tdStyle =
  [ ("padding", "20px")
  , ("width", "50%")
  ] ++ centeredStyle

trStyle =
  [ ("padding", "20px") ]


centeredStyle =
  [ ("text-align", "center") ]


buttonStyle =
  [ ("height", "40px")
  , ("border-radius", "5px")
  , ("float", "right")
  , ("margin-right", "20px")
  , ("padding-right", "20px")
  , ("padding-left", "20px")
  ]


divStyle =
  [ ("border", "3px solid")
  , ("margin", "30px auto 30px auto")
  , ("padding", "20px")
  , ("width", "50%")
  ]

h1Style = []
