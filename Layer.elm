module Layer where

import Graphics.Collage exposing (..)
import Array

import Common exposing (..)
import Tile

-- MODEL

type alias Layer =
  { size: Size,
    tiles: Array.Array (Maybe Tile.Tile)
  }

initialize : Size -> Tile.Tile -> Layer
initialize (w, h) tile =
  { size = (w, h),
    tiles = Array.repeat (w*h) (Just tile)
  }

-- UPDATE

update : Float -> Layer -> Layer
update tickTime layer =
  let
    maybeNewTile = Maybe.map (Tile.update tickTime)
    newTiles = Array.map maybeNewTile layer.tiles
  in
    { layer | tiles = newTiles }

-- VIEW

viewSize : Layer -> Size
viewSize layer =
  let
    (w,h) = layer.size
    (tileW, tileH) = Tile.defaultSize
  in
    (w*tileW, h*tileH)

view : Layer -> Form
view layer =
  let
    (w, h) = layer.size
    draw = (\i tile -> 
      let column = i % w
          row = i // h
          (tileW, tileH) = Tile.defaultSize
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) (tileView tile)
      )
  in
    Array.indexedMap draw layer.tiles
      |> Array.toList
      |> group

tileView : Maybe Tile.Tile -> Form
tileView tile =
  case tile of
    Just tile ->
      Tile.view tile
    Nothing ->
      Tile.defaultView
