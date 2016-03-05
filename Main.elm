import StartApp.Simple
import Date exposing (Date)
import Time exposing (Time)
import Dict exposing (Dict)
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
  { activities: Dict String Activity
  , createInput: String
  , page: Page
  }


type Page = HomePage | ActivityPage String

type alias Activity =
  { name: String
  , sessions: Dict Time Session
  }

type alias Session =
  { start: Date
  , finish: Date
  }

init : Model
init = { activities = Dict.empty
       , createInput = ""
       , page = HomePage
       }

initActivity : String -> Activity
initActivity name = { name = name
                    , sessions = Dict.empty
                    }

-- UPDATE

type Action = Create
            | Delete String
            | UpdateCreateInput String
            | SetPage Page
            | Nop

update : Action -> Model -> Model
update action model =
  case action of

    Create ->
      let name = model.createInput
      in { model | activities = Dict.insert name (initActivity name) model.activities
                 , createInput = "" }

    Delete activityName ->
      { model | activities = Dict.remove activityName model.activities }

    UpdateCreateInput str ->
      { model | createInput = str }

    SetPage page ->
      { model | page = page }

    Nop ->
      model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.page of
    HomePage ->
      homePageView address model
    ActivityPage activityName ->
      let maybeActivity = (Dict.get activityName model.activities)
      in
        case maybeActivity of
          Just activity ->
            activityPageView address activity
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


activityPageView : Signal.Address Action -> Activity -> Html
activityPageView address activity =
  div [ style divStyle ]
      [ text activity.name
      , button [ onClick address (SetPage HomePage) ]
               [ text "Go back" ]
      ]


activityListView : Signal.Address Action -> Dict String Activity -> Html
activityListView address activities =
  if Dict.isEmpty activities
  then
    p [ style centeredStyle ] [ text "No activities" ]
  else
    table [ style tableStyle ]
          [ tbody []
                  (List.map ((activityView address) << snd) (Dict.toList activities))
          ]


activityView : Signal.Address Action -> Activity -> Html
activityView address activity =
  tr [ style trStyle ]
     [ td [ onClick address (SetPage (ActivityPage activity.name))
          , style tdTextStyle
          ]
          [ text activity.name ]
     , td [ style tdStyle ]
          [ button [ onClick address (Delete activity.name)
                   , style buttonStyle
                   ]
                   [ text "Delete" ]
          ]
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

strDateStr : String -> Result String String
strDateStr str =
  case Date.fromString str of
    Ok date ->
      Ok (Date.Format.format "%Y %B %e %l:%M %p" date)
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
  , ("padding-right", "20px")
  , ("padding-left", "20px")
  ]


divStyle =
  [ ("border", "3px solid")
  , ("margin", "30px auto 30px auto")
  , ("padding", "20px")
  , ("width", "50%")
  ]
