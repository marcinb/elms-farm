module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Array
import Window
import Time

import Common exposing (..)
import Layer
import Tile

-- MODEL

type alias World = List Layer.Layer

worldSize : Size
worldSize = (15, 15)

initialWorld : World
initialWorld =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.Layer
groundLayer = Layer.initialize worldSize (Tile.initialize Tile.Soil)

plantsLayer : Layer.Layer
plantsLayer = Layer.initialize worldSize (Tile.initialize Tile.Grass)

-- UPDATE

update : Float -> World -> World
update tickTime world =
  List.map (Layer.update tickTime) world

-- VIEW

view : (Int, Int) -> World -> Element
view (w,h) world =
  let
    centeredLayerView : Layer.Layer -> Form
    centeredLayerView layer =
      let
        (layerW, layerH) = Layer.viewSize layer
        offsetX = -(toFloat layerW / 2.0)
        offsetY = -(toFloat layerH / 2.0)
      in
        move (offsetX, offsetY) (Layer.view layer)
  in
    collage w h (List.map centeredLayerView world)

world : Signal World
world =
  Signal.foldp update initialWorld (Time.fps 30)

main : Signal Element
main =
  Signal.map2 view Window.dimensions world

