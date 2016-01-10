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

type alias World = List Layer.Layer

type alias Game = 
  { world : World,
    mode : Mode
  }

worldSize : Size
worldSize = (15, 15)

initialWorld : World
initialWorld =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.Layer
groundLayer = Layer.initialize worldSize (Tile.initialize Tile.Soil)

plantsLayer : Layer.Layer
plantsLayer = Layer.initialize worldSize (Tile.initialize Tile.Grass)

initialGame =
  { world = initialWorld,
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
        newWorld = List.map (Layer.update delta) game.world 
      in
        { game | world = newWorld }
    ChangeMode newMode ->
      { game | mode = newMode }

-- VIEW

view : Signal.Address Mode -> Game -> Html.Html
view address game =
  let
    (w,h) = 
      Maybe.withDefault Layer.empty (List.head game.world)
        |> Layer.viewSize

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
        collage w h (List.map centeredLayerView game.world)
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

