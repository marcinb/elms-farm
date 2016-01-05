module Layer where

import Graphics.Collage exposing (..)
import Array

import Common exposing (..)
import Tile

-- MODEL:

type alias Layer =
  { size: Size,
    elements: Array.Array Tile.Tile
  }

initialize : Size -> Tile.Tile -> Layer
initialize (w, h) tile =
  { size = (w, h),
    elements = Array.repeat (w*h) tile
  }

-- VIEW:

toForm : Layer -> Form
toForm layer =
  let
    (w, h) = layer.size
    mappingFn = (\i tile -> 
      let column = i % w
          row = i // h
          (tileW, tileH) = Tile.size tile
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) (Tile.toForm tile)
      )
  in
    Array.indexedMap mappingFn layer.elements
      |> Array.toList
      |> group
