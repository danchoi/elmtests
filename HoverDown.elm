module Main where
import Graphics.Element exposing (Element, show, flow, down)
import Graphics.Input exposing (..)
import Mouse 



clickSignal : Signal.Mailbox ()
clickSignal = Signal.mailbox ()

hoverSignal : Signal.Mailbox Bool
hoverSignal = Signal.mailbox False

downSignal : Signal Bool
downSignal = Mouse.isDown

holdDownSig : Signal Bool
holdDownSig = Signal.sampleOn clickSignal.signal downSignal

hoverAndDown : Signal Bool
hoverAndDown = Signal.map2 (&&) hoverSignal.signal downSignal

view : () -> Bool -> Bool -> Bool -> Bool -> Element
view click downState holdDown hover hoverAndDownState = 
  flow down [
      show "Hover and click"
    , show ("Click: " ++ toString click)
    , show ("Down: " ++ toString downState)
    , show ("Hold Down: " ++ toString holdDown)
    , show ("Hover: " ++ toString hover)
    , show ("Hover and Down: " ++ toString hoverAndDownState)
    , hoverable (Signal.message hoverSignal.address) 
        (button (Signal.message clickSignal.address ()) "Click")
    ]

main = Signal.map5 view clickSignal.signal downSignal holdDownSig hoverSignal.signal hoverAndDown
