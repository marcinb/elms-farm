module Layer where

import Graphics.Collage exposing (..)
import Array

import TileSet

type alias Layer =
  { size: Size,
    elements: Array.Array Int
  }

type alias Size = (Int, Int)

initialize : Size -> Int -> Layer
initialize (w, h) elem =
  { size = (w, h),
    elements = Array.repeat (w*h) elem
  }

toForm : Layer -> Form
toForm layer =
  let
    (w, h) = layer.size
    mappingFn = (\i elem -> 
      let column = i % w
          row = i // h
          tile = elementToTile elem
          (tileW, tileH) = tile.size
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) tile.form
      )
  in
    Array.indexedMap mappingFn layer.elements
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
