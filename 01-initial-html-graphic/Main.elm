--Walkthrough from https://pragmaticstudio.com/blog/2014/12/19/getting-started-with-elm

import Text exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

diamond: Color -> Float -> Form
diamond color size =
  square size
    |> filled color
    |> rotate (degrees 45)

main =
  collage 200 200 [ diamond red 100,
                    diamond blue 50,
                    filled green (circle 10) ]