module SessionForm where

import Session
import Date exposing (Date)
import Time exposing (Time)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model = Maybe SessionForm

type alias SessionForm =
  { start: String
  , finish: String
  }

init : Model
init = Nothing

-- UPDATE

type Action = Open
            | Close
            | UpdateStartInput String
            | UpdateFinishInput String
            | Submit
            | Nop

update: Action -> Model -> Model
update action model =
  case action of

    Open ->
      Just { start = "", finish = "" }

    Close ->
      Nothing

    UpdateStartInput str ->
      case model of
        Just form ->
          Just { form | start = str }
        Nothing ->
          Nothing

    UpdateFinishInput str ->
      case model of
        Just form ->
          Just { form | finish = str }
        Nothing ->
          Nothing

    Submit ->
      update Close model

    Nop ->
      model


parseSession : Model -> Maybe Session.Model
parseSession model =
  case model of
    Just form ->
      case (Date.fromString form.start, Date.fromString form.finish) of
        (Ok startDate, Ok finishDate) ->
          Just { start = startDate, finish = finishDate }
        _ ->
          Nothing
    Nothing ->
      Nothing

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model of
    Just form ->
      let
        message f val = Signal.message address (f val)
      in
        div [] [ input [ placeholder "Start..."
                       , value form.start
                       , onInput (message (\str -> (UpdateStartInput str)))
                       , onEnter address Submit Nop
                       , style inputStyle
                       ]
                       []
               , strDateView form.start
               , input [ placeholder "Finish..."
                       , value form.finish
                       , onInput (message (\str -> (UpdateFinishInput str)))
                       , onEnter address Submit Nop
                       , style inputStyle
                       ]
                       []
               , strDateView form.finish
               ]
    Nothing ->
      div [] []


strDateView : String -> Html
strDateView dateStr =
  p [] [ text (case (strDateStr dateStr) of
                 Ok str -> str
                 Err str -> str)
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
  Date.Format.format "%B %e, %Y %l:%M %p" date

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
