
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

type alias Object a =
  { a |
      x : Float,
      y : Float,
      vx : Float,
      vy : Float,
  }

type alias Ball =
  Object {}

type alias Player =
  Object { score: Int }