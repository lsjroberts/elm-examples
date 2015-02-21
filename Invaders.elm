module Invaders where

{- A space invaders clone by @gelatindesign. This game exists to portray a
successful approach to architecting a game using elm and functional reactive
programming.
See: http://gelatindesign.co.uk/developing-games-in-elm/architecture
-}

import Time
import Color (..)
import Mouse
import Signal
import Signal (Signal)
import Window
import Keyboard
import Graphics.Collage (..)
import Graphics.Element (..)


-- MODEL

{- The game state is the record that represents the current situation within
the game. Here we define it as having a player, a list of enemies and a list of
bullets for both.
-}
type alias State =
    { player : Player
    , enemies : List Enemy
    , playerBullets : List Bullet
    , enemyBullets : List Bullet
    }

{- We can create extensible records for Positioned and Moveable to prevent
having to repeat ourselves for each type of actor in the game.
-}
type alias Positioned a =
    { a | x : Int
        , y : Int
    }

type alias Moveable a = Positioned
    { a | vx : Int
        , vy : Int
        , sx : Int
        , sy : Int
    }

type alias Vector =
    { x : Int
    , y : Int
    }

{- We can extend our records here to create the Player type alias, which also
has it's own additional value for lives.
-}
type alias Player = Moveable
    { lives : Int
    }

type alias Enemy = Moveable {}

type alias Bullet = Moveable {}

{- The initial state describes how the game will look on load, we add the
player at their starting point and at full health. Enemies will be appended by
the level algorithm once the player starts the game.
-}
initialState : State
initialState =
    { player =
--        { lives = 3 }
--            |> positioned 0 0
--            |> moveable 10 0
        { lives = 3
        , x = 0
        , y = 0
        , vx = 0
        , vy = 0
        , sx = 600
        , sy = 0
        }
    , enemies = []
    , playerBullets = []
    , enemyBullets = []
    }

--positioned : Int -> Int -> a -> Positioned
positioned x y a =
    { a | x <- x
        , y <- y
    }

--moveable : Int -> Int -> a -> Moveable
moveable sx sy a =
    { a | sx <- sx
        , sy <- sy
        , vx <- 0
        , vy <- 0
    }


-- UPDATE

{- The Action data type is similar to an enum and describes the possible
actions that can occur in the game. A default NoOp action is given to start the
game with no operation until the first input.
-}
type Action
    = NoOp
    | TimeDelta Float
    | PlayerMove Vector
    | PlayerFire

{- The update method enacts an action on the state.
-}
update : Action -> State -> State
update action state =
    case action of
        NoOp ->
            state

        TimeDelta dt ->
            step (dt / 1000) state

        PlayerMove {x,y} ->
            let updatePlayer player =
                    { player
                        | vx <- player.sx * x
                        , vy <- player.sy * y
                    }
            in { state | player <- updatePlayer state.player }

        PlayerFire ->
            state

{-
-}
step : Float -> State -> State
step dt ({player} as state) =
    { state
        | player <- stepPlayer dt player
    }

stepPlayer : Float -> Player -> Player
stepPlayer dt player =
    player
        |> stepMoveable dt

--stepMoveable : Float -> Moveable -> Moveable
stepMoveable dt ({x,y,vx,vy} as moveable) =
    let x' = toFloat x
        y' = toFloat y
        vx' = toFloat vx
        vy' = toFloat vy
    in
        { moveable
            | x <- x' + vx' * dt |> floor
            , y <- y' + vy' * dt |> floor
        }

-- VIEW

{- The view method generates an element containing the scene based on the given
state.
-}
view : (Int,Int) -> State -> Element
view (w,h) ({player,enemies} as state) =
    let forms =
            [ viewPlayer player
            , viewEnemies enemies
            ]
    in
        forms
            |> collage w h
            |> container w h middle
            |> color black

viewPlayer : Player -> Form
viewPlayer ({x,y} as player) =
    ngon 3 20
        |> outlined { defaultLine | color <- blue, width <- 4 }
        |> rotate (45/2)
        |> move (toFloat x, toFloat y)

viewEnemies : List Enemy -> Form
viewEnemies enemies =
    ngon 6 20
        |> outlined { defaultLine | color <- red, width <- 4 }
        |> move (toFloat 0, toFloat 100)


-- SIGNALS

{- Here we set up our signals including our main entry point. This maps the
state onto the view method along with the current window dimensions.
-}
main : Signal Element
main =
    Signal.map2 view Window.dimensions state

{- This folds the current state through the update method along with the input
signal. Each time this is called by main the result of the foldp is returned
and used in the next call.
-}
state : Signal State
state =
    Signal.foldp update initialState input

{-
-}
input : Signal Action
input =
    Signal.mergeMany
        [ Signal.map PlayerMove Keyboard.arrows
        , Signal.map PlayerMove Keyboard.wasd
        , Signal.map TimeDelta (Time.fps 60)
        ]

--actionChannel : Channel Action
--actionChannel =
--    Signal.channel NoOp