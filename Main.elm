import StartApp.Simple
import Date
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
  { activities: List String
  , createInput: String
  , deleteInput: String
  , dateInput: String
  }


init : Model
init = { activities = []
       , createInput = ""
       , deleteInput = ""
       , dateInput = ""
       }


-- UPDATE

type Action = Create
            | Delete
            | Reset
            | UpdateCreateInput String
            | UpdateDeleteInput String
            | UpdateDateInput String

update : Action -> Model -> Model
update action model =
  case action of
    Reset ->
      init

    Create ->
      { model | activities = model.activities ++ [model.createInput]
              , createInput = "" }

    Delete ->
      { model | activities = List.filter ((/=) model.deleteInput) model.activities
              , deleteInput = "" }

    UpdateCreateInput str ->
      { model | createInput = str }

    UpdateDeleteInput str ->
      { model | deleteInput = str }

    UpdateDateInput str ->
      { model | dateInput = str }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let activities = List.map (\activity -> div [ divStyle ] [ text activity ]) model.activities
      message f value = Signal.message address (f value)
      createInput = input [ placeholder "Activity"
                          , value model.createInput
                          , onInput (message UpdateCreateInput) ]
                          []
      create = button [ onClick address (Create) ] [ text "Submit" ]
      deleteInput = input [ placeholder "Activity"
                          , value model.deleteInput
                          , onInput (message UpdateDeleteInput) ]
                          []
      delete = button [ onClick address (Delete) ] [ text "Delete" ]
      reset = button [ onClick address Reset] [ text "Reset"]
      dateInput = input [ placeholder "Date"
                          , value model.dateInput
                          , onInput (message UpdateDateInput) ]
                          []
      dateOutput = text (strDateStr model.dateInput)
  in div [] ([ div [ divStyle ] [ createInput, create ]
             , div [ divStyle ] [ deleteInput, delete ]
             , div [ divStyle ] [ reset ] ]
             ++ activities
             ++ [ div [ divStyle ] [ dateInput, dateOutput] ])


onInput : (String -> Signal.Message) -> Attribute
onInput f =
  on "input" targetValue f


strDateStr : String -> String
strDateStr str =
  case Date.fromString str of
    Ok date ->
      Date.Format.format "%Y %B %e" date
    Err error ->
      error


divStyle : Attribute
divStyle =
  style
    [ ("border", "3px solid")
    , ("margin", "30px auto 30px auto")
    , ("padding", "20px")
    , ("width", "50%") ]
