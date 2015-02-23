module Game.View where

import Game.Model (..)
import Game.Controller (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import Text (asText)

view : (Int, Int) -> CharModel -> Element
view (w,h) {x,y,dx,dy,isAtk} =
	let atkSqr a =
		if a then [move (x,y) (alpha 0.5 (filled red (square 50)))]
			 else []
	in
    container w h middle <|
    collage w h
	<| List.append
    <| [ move (x,y) (filled blue (square 30)),
         move (0,-100) (toForm (asText ("Press <Shift> to sprint and <Arrow Keys> to move")))
       ]
	<| atkSqr
