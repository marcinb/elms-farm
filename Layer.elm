module Layer where

import Graphics.Collage exposing (..)
import Array

import Common exposing (..)
import Tile

-- MODEL:

type alias Layer =
  { size: Size,
    elements: Array.Array (Maybe Tile.Tile)
  }

initialize : Size -> Tile.Tile -> Layer
initialize (w, h) tile =
  { size = (w, h),
    elements = Array.repeat (w*h) (Just tile)
  }

-- VIEW:

toForm : Layer -> Form
toForm layer =
  let
    (w, h) = layer.size
    mappingFn = (\i tile -> 
      let column = i % w
          row = i // h
          (tileW, tileH) = Tile.defaultSize
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) (tileForm tile)
      )
  in
    Array.indexedMap mappingFn layer.elements
      |> Array.toList
      |> group

tileForm : Maybe Tile.Tile -> Form
tileForm tile =
  case tile of
    Just tile ->
      Tile.toForm tile
    Nothing ->
      Tile.emptyTileForm
