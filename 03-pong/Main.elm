import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window

--INPUTS

type alias Input =
  { space: Bool
  , paddle1: Int
  , paddle2: Int
  , delta: Time
  }

delta : Signal Time
delta =
  Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.space
      (Signal.map .y Keyboard.wasd)
      (Signal.map .y Keyboard.arrows)
      delta

--MODEL

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

type State = Play | Pause

type alias Ball =
  { x : Float
  , y: Float
  , vx: Float
  , vy: Float
  }

type alias Player =
  { x : Float
  , y: Float
  , vx: Float
  , vy: Float
  , score: Int
  }


type alias Game =
  { state: State
  , ball: Ball
  , player1: Player
  , player2: Player
  }

player : Float -> Player
player x =
  Player x 0 0 0 0

defaultGame: Game
defaultGame =
  { state   = Pause
  , ball    = Ball 0 0 200 200
  , player1 = player (20-halfWidth)
  , player2 = player (halfWidth-20)
  }

--UPDATE

update : Input -> Game -> Game
update {space,dir1,dir2,delta} ({state,ball,player1,player2} as game) =
  let
    score1 =
      if ball.x > halfWidth then 1 else 0

    score2 =
      if ball.x < -halfWidth then 1 else 0

    newState =
      if space then
          Play

      else if score1 /= score2 then
          Pause

      else
          state

    newBall =
      if state == Pause then
        ball
      else
        updateBall delta ball player1 player2
  in
    { game |
        state = newState,
        ball = newBall,
        player1 = updatePlayer delta dir1 score1 player1,
        player2 = updatePlayer delta dir2 score2 player2
    }


updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall dt ball paddle1 paddle2 =
  if not (near 0 halfWidth ball.x) then
    { ball | x = 0, y = 0 }
  else
    physicsUpdate dt
      { ball |
          vx = stepV ball.vx (within paddle1 ball) (within paddle2 ball),
          vy = stepV ball.vy (ball.y < 7 - halfHeight) (ball.y > halfHeight - 7)
      }

updatePlayer : Time -> Int -> Int -> Player -> Player
updatePlayer dt dir points player =
  let
    movedPlayer =
      physicsUpdate dt { player | vy = toFloat dir * 200}
  in
    { movedPlayer | 
        y = clamp (22-halfHeight) (halfWidth-22) movedPlayer.y,
        score = player.score + points
    }

physicsUpdate dt obj =
  { obj |
      x = obj.x + obj.vx * dt,
      y = obj.y + obj.vy * dt
  }

near k c n =
  n >= k-c && n <= k+c

within paddle ball =
  near paddle.x 8 ball.x && near paddle.y 20 ball.y

stepV v lowerCollision upperCollision =
  if lowerCollision then
    abs v
  else if upperCollision then
      -(abs v)
  else
      v

--VIEW

