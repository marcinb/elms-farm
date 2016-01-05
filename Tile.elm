module Tile where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color

import Common exposing (..)

-- MODEL:

type Tile =
  Soil | Grass

-- VIEW:

defaultSize : Size
defaultSize = (32, 32)

view : Tile -> Form
view tile =
  let
    path = tileSheetPath tile
    (w,h) = defaultSize
    (x,y) = (0,5)
  in
    croppedImage (x * w, y * h) w h path
      |> toForm

defaultView : Form
defaultView =
  let
    (w,h) = defaultSize 
  in
    rect (toFloat w) (toFloat h)
      |> filled (Color.rgba 0 0 0 0)

tileSheetPath : Tile -> String
tileSheetPath tile =
  let
    path filename =
      "assets/tiles/" ++ filename
  in
    case tile of
      Soil ->
        path "plowed_soil.png"
      Grass ->
        path "tall_grass.png"
