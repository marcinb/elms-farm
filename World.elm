module World (World, initialize, update, view) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Array

import Common exposing (..)
import Layer

-- MODEL

type alias World = 
  { size : Size,
    layers : List Layer.Layer
  }

initialize : Size -> World
initialize size =
  { size = size,
    layers =
      [ Layer.soil size, 
        Layer.grass size
      ]
  }

-- UPDATE

update : Float -> World -> World
update deltaTime world =
  let
    updatedLayers = List.map (Layer.update deltaTime) world.layers
  in
    { world | layers = updatedLayers }

-- VIEW
view : World -> Element
view world =
  let
    (w,h) = Layer.viewSize (Layer.empty world.size)

    centeredLayerView : Layer.Layer -> Form
    centeredLayerView layer =
      let
        (layerW, layerH) = Layer.viewSize layer
        offsetX = -(toFloat layerW / 2.0)
        offsetY = -(toFloat layerH / 2.0)
      in
        move (offsetX, offsetY) (Layer.view layer)
  in
    collage w h (List.map centeredLayerView world.layers)
