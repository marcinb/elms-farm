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
    path = tilePath tile
    (w,h) = defaultSize
  in
    fittedImage w h path
      |> toForm

tilePath : Tile -> String
tilePath tile =
  let
    path filename =
      "assets/tiles/" ++ filename
  in
    case tile.kind of
      Soil ->
        path "soil_0.png"
      Grass ->
        path "grass_" ++ toString  (lifePhase tile) ++ ".png"

