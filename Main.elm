module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Array
import Window

import Layer

type alias Size = (Int, Int)
type alias Map = List Layer.MapLayer

mapSize : Size
mapSize = (10, 10)

initialMap : Map
initialMap =
  [ groundLayer, plantsLayer ]

groundLayer : Layer.MapLayer
groundLayer = Layer.fillLayer mapSize 1

plantsLayer : Layer.MapLayer
plantsLayer = 
  let
    (mapW, mapH) = mapSize
    initFn = (\i ->
      if List.member i [4,13,33,34,35,53,60,69]
      then 2
      else 0
    )
  in
    Array.initialize (mapW*mapH) initFn

-- VIEW

mapToForms : Map -> List Form
mapToForms m =
  List.map (Layer.layerToForm mapSize) m

view : (Int, Int) -> Element
view (w,h) =
    collage w h (mapToForms initialMap)

main : Signal Element
main =
  Signal.map view Window.dimensions

