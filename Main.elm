module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input as Input
import Array
import Window
import Time
import Html

import Common exposing (..)
import Layer
import Tile

-- MODEL

type Mode = Play | Edit

type alias World = 
  { size : Size,
    layers : List Layer.Layer
  }

type alias Game = 
  { world : World,
    mode : Mode
  }

initialWorld : Size -> World
initialWorld size =
  { size = size,
    layers =
      [ Layer.soil size, 
        Layer.grass size
      ]
  }

initialGame =
  { world = initialWorld (15, 15),
    mode = Play
  }

-- UPDATE

type Action = 
  Tick Float
  | ChangeMode Mode

update : Action -> Game -> Game
update action game =
  case action of
    Tick delta ->
      let 
        world = game.world
        newWorld = { world | layers = List.map (Layer.update delta) world.layers }
      in
        { game | world = newWorld }
    ChangeMode newMode ->
      { game | mode = newMode }

-- VIEW

view : Signal.Address Mode -> Game -> Html.Html
view address game =
  let
    (w,h) = Layer.viewSize (Layer.empty game.world.size)

    centeredLayerView : Layer.Layer -> Form
    centeredLayerView layer =
      let
        (layerW, layerH) = Layer.viewSize layer
        offsetX = -(toFloat layerW / 2.0)
        offsetY = -(toFloat layerH / 2.0)
      in
        move (offsetX, offsetY) (Layer.view layer)
  in
    Html.div []
      [ Html.h1 [] 
        [ show game.mode
            |> Html.fromElement
        ],
        collage w h (List.map centeredLayerView game.world.layers)
          |> Html.fromElement,
        toggleModeButton game.mode address
          |> Html.fromElement
      ]

toggleModeButton : Mode -> Signal.Address Mode -> Element
toggleModeButton mode address =
  let
    newMode = case mode of
      Play -> Edit
      _ -> Play

    msg = Signal.message address newMode
  in
    Input.button msg "Toggle Mode"

-- SIGNALS

inputMailbox : Signal.Mailbox Mode
inputMailbox = Signal.mailbox Play

modeChange : Signal Action
modeChange = Signal.map ChangeMode inputMailbox.signal

everyTick : Signal Action
everyTick =
  Signal.map Tick (Time.fps 30)

gameChange : Signal Game
gameChange =
  let
    input = Signal.mergeMany [everyTick, modeChange]
  in
    Signal.foldp update initialGame input

main : Signal Html.Html
main =
  Signal.map (view inputMailbox.address) gameChange

