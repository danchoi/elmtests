module Main where
import Graphics.Element exposing (Element, show, flow, down)
import Graphics.Input exposing (..)
import Mouse 
import Time exposing (..)


clickSignal : Signal.Mailbox ()
clickSignal = Signal.mailbox ()


timer : Signal Time
timer = every second

holdDownTime : Signal Float
holdDownTime = Signal.map (Time.inSeconds) 
    <| Signal.foldp (+) 0 
    <| Time.fpsWhen 10 hoverAndDown
    


hoverSignal : Signal.Mailbox Bool
hoverSignal = Signal.mailbox False

downSignal : Signal Bool
downSignal = Mouse.isDown

holdDownSig : Signal Bool
holdDownSig = Signal.sampleOn clickSignal.signal downSignal

hoverAndDown : Signal Bool
hoverAndDown = Signal.map2 (&&) hoverSignal.signal downSignal

view : Float -> Bool -> Bool -> Bool -> Bool -> Element
view time downState holdDown hover hoverAndDownState = 
  flow down [
      show "Hover and click"
    , show ("Down: " ++ toString downState)
    , show ("Hold Down: " ++ toString holdDown)
    , show ("Hover: " ++ toString hover)
    , show ("Hover and Down: " ++ toString hoverAndDownState)
    , hoverable (Signal.message hoverSignal.address) 
        (button (Signal.message clickSignal.address ()) "Click")
    , show ("Time elapsed hoverAndDown " ++ toString  time)
    ]

main = Signal.map5 view holdDownTime downSignal holdDownSig hoverSignal.signal hoverAndDown
              
