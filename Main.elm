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

initialLayer : MapLayer
initialLayer =
  let
    (mapW, mapH) = mapSize
  in
    Array.repeat (mapW*mapH) 0

-- VIEW

layerElementToTile : Int -> TileSet.Tile
layerElementToTile layerElem =
  case layerElem of
    0 ->
      TileSet.tile TileSet.plowedSoilTiles (0,5)
    _ ->
      TileSet.tile TileSet.grassTiles (2,5)

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


view : (Int, Int) -> Element
view (w,h) =
    collage w h [ layerToForm initialLayer ]

main : Signal Element
main =
  Signal.map view Window.dimensions

