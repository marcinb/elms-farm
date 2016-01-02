module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)

import Keyboard
import Window
import Time

import Array

type alias TileSet = 
  { path : String,
    tileSize: (Int, Int)
  }
   
defaultTileSize : (Int, Int) 
defaultTileSize =
  (32, 32)

createTileSet : String -> TileSet
createTileSet filename =
  let
    tileSetPath = "assets/tiles/" ++ filename
  in
     { path = tileSetPath,
       tileSize = defaultTileSize
     }

plowedSoilTiles : TileSet
plowedSoilTiles = createTileSet "plowed_soil.png"

mapWidth : Int
mapWidth = 10

mapHeight : Int
mapHeight = 10

type alias Map = Array.Array Int

defaultMap : Map
defaultMap =
  Array.repeat (mapWidth*mapHeight) 0

-- VIEW

mapToForms : Map -> List Form
mapToForms gameMap =
  let
    groundTile = tile plowedSoilTiles (0,5)
    (tileW, tileH) = (32.0, 32.0)
    mappingFn = (\i _ -> 
      let column = i % mapWidth
          row = i // mapHeight
          offsetX = (toFloat column) * tileW
          offsetY = (toFloat row) * tileH
      in
        move (offsetX, offsetY) groundTile
      )
  in
    Array.indexedMap mappingFn gameMap
      |> Array.toList


tile : TileSet -> (Int, Int) -> Form
tile tileSet (x,y) =
  let
    (w,h) = tileSet.tileSize
  in
    croppedImage (x * w, y * h) w h tileSet.path
      |> toForm

view : (Int, Int) -> Element
view (w,h) =
    collage w h (mapToForms defaultMap)

main : Signal Element
main =
  Signal.map view Window.dimensions

