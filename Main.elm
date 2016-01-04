module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Array
import Window

import Layer

type alias Size = (Int, Int)
type alias Map = List Layer.Layer

mapSize : Size
mapSize = (10, 10)

initialMap : Map
initialMap =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.Layer
groundLayer = Layer.initialize mapSize 1

plantsLayer : Layer.Layer
plantsLayer = Layer.initialize mapSize 2

-- VIEW

mapToForms : Map -> List Form
mapToForms m =
  List.map Layer.toForm m

view : (Int, Int) -> Element
view (w,h) =
    collage w h (mapToForms initialMap)

main : Signal Element
main =
  Signal.map view Window.dimensions

