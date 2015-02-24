module Game.View where

import Game.Model (..)
import Game.Controller (..)
import Game.Util (..)
import Game.Collision (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Text (asText)

view : (Int, Int) -> CharModel -> Element
view (w,h) {x,y,dx,dy,atks,dir,col} =
    let
        xOff = 25
        yOff = 25
        inDir (x',y') d = case d of
            Right -> (x' + xOff,y')
            Left  -> (x' - xOff,y')
            Up    -> (x',y' + yOff)
            Down  -> (x',y' - yOff)
        atkSqr a = case a of
            Atk t  -> [move (inDir (x,y) dir) (alpha (lerp 1 0 atkTime t 0) (filled red (square 50)))]
            CanAtk -> []
    in
    container w h middle <|
    collage w h
    <| List.append
        [ move (x,y) (filled blue (rect col.w col.h)),
          move (0,-100) (toForm (asText ("Press <Shift> to sprint, <Arrow Keys> to move, <Space> to Attack")))
        ]
    <| atkSqr atks
