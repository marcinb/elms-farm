module Tile where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color

import Common exposing (..)

-- MODEL

type alias LifePhase = Int

type Tile =
  Soil | Grass LifePhase

-- VIEW

defaultSize : Size
defaultSize = (32, 32)

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
    case tile of
      Soil ->
        path "plowed_soil.png"
      Grass _ ->
        path "tall_grass.png"

positionInSheet : Tile -> (Int, Int)
positionInSheet tile =
  case tile of
    Soil ->
      (0,5)
    Grass phase ->
      (phase, 5)

defaultView : Form
defaultView =
  let
    (w,h) = defaultSize 
  in
    rect (toFloat w) (toFloat h)
      |> filled (Color.rgba 0 0 0 0)
