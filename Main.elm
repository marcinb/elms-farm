module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Window

import Array

import TileSet

type alias Size = (Int, Int)

mapSize : Size
mapSize = (10, 10)

type alias MapLayer = Array.Array Int
type alias Map = List MapLayer

fillLayer : Int -> MapLayer
fillLayer layerElem =
  let
    (mapW, mapH) = mapSize
  in
    Array.repeat (mapW*mapH) layerElem

groundLayer : MapLayer
groundLayer = fillLayer 1

plantsLayer : MapLayer
plantsLayer = fillLayer 2

initialMap : Map
initialMap =
  [ groundLayer, plantsLayer ]

-- VIEW

layerElementToTile : Int -> TileSet.Tile
layerElementToTile layerElem =
  case layerElem of
    1 ->
      TileSet.tile TileSet.plowedSoilTiles (0,5)
    2 ->
      TileSet.tile TileSet.grassTiles (2,5)
    _ ->
      TileSet.tile TileSet.plowedSoilTiles (0,5)

layerToForm : MapLayer -> Form
layerToForm layer =
  let
    (mapW, mapH) = mapSize
    mappingFn = (\i layerElem -> 
      let column = i % mapW
          row = i // mapH
          tile = layerElementToTile layerElem
          (tileW, tileH) = tile.size
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) tile.form
      )
  in
    Array.indexedMap mappingFn layer
      |> Array.toList
      |> group

mapToForms : Map -> List Form
mapToForms m =
  List.map layerToForm m

view : (Int, Int) -> Element
view (w,h) =
    collage w h (mapToForms initialMap)

main : Signal Element
main =
  Signal.map view Window.dimensions

