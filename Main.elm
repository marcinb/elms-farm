module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Keyboard
import Window
import Time

type alias TileSet = 
  { path : String,
    tileSize: (Int, Int)
  }
   
createTileSet : String -> TileSet
createTileSet filename =
  let
    tileSetPath = "assets/tiles/" ++ filename
  in
     { path = tileSetPath,
       tileSize = (32, 32)
     }

plowedSoilTiles = createTileSet "plowed_soil.png"

-- VIEW

tile : TileSet -> (Int, Int) -> Form
tile tileSet (x,y) =
  let
    (w,h) = tileSet.tileSize
  in
    croppedImage (x * w, y * h) w h tileSet.path
      |> toForm

view : (Int, Int) -> Element
view (w,h) =
    collage w h
      [tile plowedSoilTiles (0,5)]

main : Signal Element
main =
  Signal.map view Window.dimensions

