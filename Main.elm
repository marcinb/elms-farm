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

type alias Map = List Layer.Layer

mapSize : Size
mapSize = (10, 10)

initialMap : Map
initialMap =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.Layer
groundLayer = Layer.initialize mapSize Tile.Soil

plantsLayer : Layer.Layer
plantsLayer = Layer.initialize mapSize (Tile.Grass 1)

-- UPDATE

update : Float -> Map -> Map
update tickTime map =
  List.map (Layer.update tickTime) map

-- VIEW

mapView : Map -> List Form
mapView m =
  List.map Layer.view m

view : Map -> (Int, Int) -> Float -> Element
view map (w,h) tickTime =
  let
    newMap = update tickTime map
  in
    collage w h (mapView newMap)

main : Signal Element
main =
  Signal.map2 (view initialMap) Window.dimensions (Time.fps 30)

