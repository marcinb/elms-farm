module Tile where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Time

import Common exposing (..)

-- MODEL

type alias LifePhase = Int

type Kind =
  Soil | Grass

type alias Tile =
  { kind : Kind,
    bornAt : Float,
    lifeTime : Float
  }

initialize : Kind -> Tile
initialize kind =
  { kind = kind,
    bornAt = 0.0,
    lifeTime = 0.0
  }

lifePhase : Tile -> Int
lifePhase tile =
  if
    tile.lifeTime > 15 * Time.second
  then 
    2
  else if
    tile.lifeTime > 5 * Time.second
  then 
    1
  else 
    0

-- UPDATE

update : Float -> Tile -> Tile
update tickTime tile =
  { tile | lifeTime = tile.lifeTime + tickTime }
    
-- VIEW

defaultSize : Size
defaultSize = (32, 32)

-- Empty tile, render as transparent rect.
defaultView : Form
defaultView =
  let
    (w,h) = defaultSize 
  in
    rect (toFloat w) (toFloat h)
      |> filled (Color.rgba 0 0 0 0)

view : Tile -> Form
view tile =
  let
    path = sheetPath tile
    (w,h) = defaultSize
    (x,y) = positionInSheet tile
  in
    croppedImage (x * w, y * h) w h path
      |> toForm

sheetPath : Tile -> String
sheetPath tile =
  let
    path filename =
      "assets/tiles/" ++ filename
  in
    case tile.kind of
      Soil ->
        path "plowed_soil.png"
      Grass ->
        path "tall_grass.png"

positionInSheet : Tile -> (Int, Int)
positionInSheet tile =
  case tile.kind of
    Soil ->
      (0,5)
    Grass ->
      ((lifePhase tile), 5)
