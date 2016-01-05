module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Array
import Window

import Common exposing (..)
import Layer
import Tile

type alias Map = List Layer.Layer

mapSize : Size
mapSize = (10, 10)

initialMap : Map
initialMap =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.Layer
groundLayer = Layer.initialize mapSize Tile.Soil

plantsLayer : Layer.Layer
plantsLayer = Layer.initialize mapSize Tile.Grass

-- VIEW

mapView : Map -> List Form
mapView m =
  List.map Layer.view m

view : (Int, Int) -> Element
view (w,h) =
    collage w h (mapView initialMap)

main : Signal Element
main =
  Signal.map view Window.dimensions

