module TileSet where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

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

tile : TileSet -> (Int, Int) -> Form
tile tileSet (x,y) =
  let
    (w,h) = tileSet.tileSize
  in
    croppedImage (x * w, y * h) w h tileSet.path
      |> toForm

