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
    (tileW, tileH) = groundTile.size
    mappingFn = (\i _ -> 
      let column = i % mapWidth
          row = i // mapHeight
          offsetX = (toFloat column) * (toFloat tileW)
          offsetY = (toFloat row) * (toFloat tileH)
      in
        move (offsetX, offsetY) groundTile.form
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

