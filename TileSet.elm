module TileSet where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color

import Common exposing (..)

type alias TileSet = 
  { path : String,
    tileSize: Size
  }

type alias Tile =
  { size: Size,
    form: Form
  }
   
initialize : String -> TileSet
initialize filename =
  let
    tileSetPath = "assets/tiles/" ++ filename
    defaultTileSize = (32, 32)
  in
     { path = tileSetPath,
       tileSize = defaultTileSize
     }

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

emptyTile : Tile
emptyTile =
  { size = (32, 32),
    form = rect 32 32
      |> filled (Color.rgba 0 0 0 0)
  }

plowedSoilTiles : TileSet
plowedSoilTiles = initialize "plowed_soil.png"
grassTiles : TileSet
grassTiles = initialize "tall_grass.png"
