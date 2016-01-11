module Farm where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input as Input
import Window
import Time
import Html

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

update : Action -> Game -> Game
update action game =
  case action of
    Tick delta ->
        { game | world = World.update delta game.world }
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

everyTick : Signal Action
everyTick =
  Signal.map Tick (Time.fps 30)

gameChange : Signal Game
gameChange =
  let
    input = Signal.mergeMany [everyTick, modeChange]
  in
    Signal.foldp update initialGame input

main : Signal Html.Html
main =
  Signal.map (view inputMailbox.address) gameChange

