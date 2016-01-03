module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Window

import Array

import TileSet

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
    groundTile = TileSet.tile TileSet.plowedSoilTiles (0,5)
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


view : (Int, Int) -> Element
view (w,h) =
    collage w h (mapToForms defaultMap)

main : Signal Element
main =
  Signal.map view Window.dimensions

