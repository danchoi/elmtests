module Main where
import Mouse 
import Time exposing (..)
import StartApp exposing (..)
import Task exposing (Task)
import Signal exposing (Address, message)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseOver)


type alias Model = 
    { hoverAndDownState : Bool
    , hoverAndDownSecs : Float 
    }

type Action = HoverAndDown Bool | HoverAndDownTime  Float

model0 : Model 
model0 = Model False 0

hoverSignal : Signal.Mailbox Bool
hoverSignal = Signal.mailbox False


hoverAndDown : Signal Bool
hoverAndDown = Signal.map2 (&&) hoverSignal.signal Mouse.isDown

hoverAndDown' : Signal Action
hoverAndDown' = hoverAndDown |> Signal.map (HoverAndDown)


hoverAndDownTime : Signal Action 
hoverAndDownTime = Signal.map (HoverAndDownTime << Time.inSeconds) 
      <| Signal.foldp (+) 0 
      <| Time.fpsWhen 10 hoverAndDown

    
update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    HoverAndDown True -> ({model | hoverAndDownState = True}, Effects.none)
    HoverAndDown False -> ({model | hoverAndDownState = False}, Effects.none)
    HoverAndDownTime t -> ({model | hoverAndDownSecs = t}, Effects.none)

init : (Model, Effects Action)
init = (model0, Effects.none)


-- http://package.elm-lang.org/packages/evancz/elm-html/4.0.2/Html-Events

view : Address Action -> Model -> Html
view address model = 
    div [] 
      [ p [] [ text <|   "Hover and down: " ++ toString model.hoverAndDownState ]
      , p [] [ text <| "Hover and down seconds elapsed: " ++ toString model.hoverAndDownSecs ]
      , p  [onMouseOver hoverSignal.address True]  [ text "Press" ]
      ]


inputs : List (Signal Action)
inputs = [hoverAndDown', hoverAndDownTime]

appConfig : Config Model Action
appConfig = Config init update view inputs 

app = StartApp.start appConfig

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks


