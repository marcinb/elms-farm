module Layer where

import Graphics.Collage exposing (..)

import Array
import TileSet

-- LAYER

type alias Layer = Array.Array Int
type alias Size = (Int, Int)

initialize : Size -> Int -> Layer
initialize (w, h) layerElem =
  Array.repeat (w*h) layerElem

toForm : Size -> Layer -> Form
toForm size layer =
  let
    (w, h) = size
    mappingFn = (\i layerElem -> 
      let column = i % w
          row = i // h
          tile = elementToTile layerElem
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

elementToTile : Int -> TileSet.Tile
elementToTile elem =
  case elem of
    1 ->
      TileSet.tile TileSet.plowedSoilTiles (0,5)
    2 ->
      TileSet.tile TileSet.grassTiles (2,5)
    _ ->
      TileSet.emptyTile
