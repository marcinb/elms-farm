module Layer (Layer, empty, soil, grass, update, view, viewSize) where

import Graphics.Collage exposing (..)
import Array

import Common exposing (..)
import Tile

-- MODEL

type alias Layer =
  { size : Size,
    tiles : Array.Array (Maybe Tile.Tile),
    tileSize : Size
  }

initialize : Maybe Tile.Tile -> Size -> Layer
initialize tile (w,h) =
  { size = (w, h),
    tiles = Array.repeat (w*h) tile,
    tileSize = (32,32)
  }

empty : Size -> Layer
empty = 
  initialize Nothing

soil : Size -> Layer
soil =
  initialize (Just Tile.soil)

grass : Size -> Layer
grass = 
  initialize (Just Tile.grass)

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
    (tileW, tileH) = layer.tileSize
  in
    (w*tileW, h*tileH)

view : Layer -> Form
view layer =
  let
    (w, h) = layer.size
    defaultView = Tile.defaultView layer.tileSize
    draw : Int -> Maybe Tile.Tile -> Form
    draw = (\i tile -> 
      let column = i % w
          row = i // h
          (tileW, tileH) = layer.tileSize
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) (tileViewWithDefault defaultView tile)
      )
  in
    Array.indexedMap draw layer.tiles
      |> Array.toList
      |> group

tileViewWithDefault : Form -> Maybe Tile.Tile -> Form
tileViewWithDefault default tile =
  case tile of
    Just tile ->
      Tile.view tile
    Nothing ->
      default
