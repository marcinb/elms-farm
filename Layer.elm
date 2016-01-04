module Layer where

import Graphics.Collage exposing (..)

import Array
import TileSet

-- LAYER

type alias MapLayer = Array.Array Int
type alias Size = (Int, Int)

fillLayer : Size -> Int -> MapLayer
fillLayer (w, h) layerElem =
  Array.repeat (w*h) layerElem

layerElementToTile : Int -> TileSet.Tile
layerElementToTile layerElem =
  case layerElem of
    1 ->
      TileSet.tile TileSet.plowedSoilTiles (0,5)
    2 ->
      TileSet.tile TileSet.grassTiles (2,5)
    _ ->
      TileSet.emptyTile

layerToForm : Size -> MapLayer -> Form
layerToForm size layer =
  let
    (w, h) = size
    mappingFn = (\i layerElem -> 
      let column = i % w
          row = i // h
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
