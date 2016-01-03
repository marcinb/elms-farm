module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Window

import Array

import TileSet

type alias Size = (Int, Int)

mapSize : Size
mapSize = (10, 10)

type alias Map = Array.Array Int

defaultMap : Map
defaultMap =
  let
    (mapW, mapH) = mapSize
  in
    Array.repeat (mapW*mapH) 0

-- VIEW

mapToForms : Map -> List Form
mapToForms gameMap =
  let
    groundTile = TileSet.tile TileSet.plowedSoilTiles (0,5)
    (tileW, tileH) = groundTile.size
    (mapW, mapH) = mapSize
    mappingFn = (\i _ -> 
      let column = i % mapW
          row = i // mapH
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

