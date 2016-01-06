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
worldSize = (10, 10)

initialWorld : World
initialWorld =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.Layer
groundLayer = Layer.initialize worldSize Tile.Soil

plantsLayer : Layer.Layer
plantsLayer = Layer.initialize worldSize (Tile.Grass 1)

-- UPDATE

update : Float -> World -> World
update tickTime world =
  List.map (Layer.update tickTime) world

-- VIEW

worldView : World -> List Form
worldView world =
  List.map Layer.view world

view : World -> (Int, Int) -> Float -> Element
view world (w,h) tickTime =
  let
    newWorld = update tickTime world
  in
    collage w h (worldView newWorld)

main : Signal Element
main =
  Signal.map2 (view initialWorld) Window.dimensions (Time.fps 30)

