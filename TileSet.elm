module TileSet where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

type alias Size = (Int, Int)

type alias TileSet = 
  { path : String,
    tileSize: Size
  }

type alias Tile =
  { size: Size,
    form: Form
  }
   
defaultTileSize : Size
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

tile : TileSet -> (Int, Int) -> Tile
tile tileSet (x,y) =
  let
    (w,h) = tileSet.tileSize
    form = 
      croppedImage (x * w, y * h) w h tileSet.path
        |> toForm
  in
    { size = tileSet.tileSize,
      form = form
    }
