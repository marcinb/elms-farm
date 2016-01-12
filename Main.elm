module Farm where

import Graphics.Element exposing (Element, show)
import Graphics.Collage exposing (..)
import Graphics.Input as Input
import Window
import Time
import Html
import Mouse

import Common exposing (..)
import World

-- MODEL

type Mode = Play | Edit

type alias Game = 
  { world : World.World,
    mode : Mode
  }

initialGame =
  { world = World.initialize (15, 15),
    mode = Play
  }

-- UPDATE

type Action = 
  Tick Float
  | ChangeMode Mode
  | Click Position

update : Action -> Game -> Game
update action game =
  case action of
    Tick delta ->
      { game | world = World.update delta game.world }
    Click (x,y) ->
      game
    ChangeMode newMode ->
      { game | mode = newMode }

-- VIEW

view : Signal.Address Mode -> Game -> Html.Html
view address game =
  Html.div []
    [ Html.h1 [] 
      [ show game.mode
          |> Html.fromElement
      ],
      World.view game.world
        |> Html.fromElement,
      toggleModeButton game.mode address
        |> Html.fromElement
    ]

toggleModeButton : Mode -> Signal.Address Mode -> Element
toggleModeButton mode address =
  let
    newMode = case mode of
      Play -> Edit
      _ -> Play

    msg = Signal.message address newMode
  in
    Input.button msg "Toggle Mode"

-- SIGNALS

inputMailbox : Signal.Mailbox Mode
inputMailbox = Signal.mailbox Play

modeChange : Signal Action
modeChange = Signal.map ChangeMode inputMailbox.signal

ticks : Signal Action
ticks =
  Signal.map Tick (Time.fps 30)

clicks : Signal Action
clicks =
  let
    clicksPosition = Signal.sampleOn Mouse.clicks Mouse.position
  in
    Signal.map Click clicksPosition

gameChange : Signal Game
gameChange =
  let
    input = Signal.mergeMany [ticks, clicks, modeChange]
  in
    Signal.foldp update initialGame input

main : Signal Html.Html
main =
  Signal.map (view inputMailbox.address) gameChange

