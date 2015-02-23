module Game.View where

import Game.Model (..)
import Game.Controller (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Text (asText)

view : (Int, Int) -> CharModel -> Element
view (w,h) {x,y,dx,dy} =
    container w h middle <|
    collage w h
        [ move (x,y) (filled blue (square 30)),
          move (0,-100) (toForm (asText ("Press <Shift> to sprint and <Arrow Keys> to move")))
        ]
