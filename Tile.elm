module Tile where

import Graphics.Element exposing (..)
import Graphics.Collage as Collage
import Color

import Common exposing (..)

-- MODEL:

type Tile =
  Empty 
  | Soil 
  | Grass

-- VIEW:

toForm : Tile -> Collage.Form
toForm tile =
  let
    path = tileSheetPath tile
    (w,h) = size tile
    (x,y) = (0,5)
  in
    case path of
      Just path ->
        croppedImage (x * w, y * h) w h path
          |> Collage.toForm
      Nothing ->
        emptyTileForm

emptyTileForm : Collage.Form
emptyTileForm =
  let
    (w,h) = size Empty
  in
    Collage.rect (toFloat w) (toFloat h)
      |> Collage.filled (Color.rgba 0 0 0 0)

size : Tile -> Size
size _ = (32, 32)

tileSheetPath : Tile -> Maybe String
tileSheetPath tile =
  let
    path filename =
      "assets/tiles/" ++ filename
  in
    case tile of
      Soil ->
        Just (path "plowed_soil.png")
      Grass ->
        Just (path "tall_grass.png")
      _ ->
        Nothing
