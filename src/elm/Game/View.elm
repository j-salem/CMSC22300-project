module Game.View where

import Game.Model (..)
import Game.Controller (..)
import Game.Util (..)
import Game.Collision (..)
import Game.Quadtree (..)
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Array as A
import List
import Text (asText)

view : (Int, Int) -> GameState -> Element
view (w,h) {player,currentRoom} =
    let
        {x,y,dx,dy,atks,dir,col} = player
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
        [ move (x,y) (filled blue (rect col.width col.height)),
          move (0,-100) (toForm (asText ("Press <Shift> to sprint, <Arrow Keys> to move, <Space> to Attack")))
        ]
    <| atkSqr atks ++ drawQuadtree currentRoom.contents


-- QT draw code

drawQuadtree : Quadtree (Positioned a) -> List (Form)
drawQuadtree qt = case qt of
    Leaf _ _ ar        -> drawMany ar
    Node _ nw ne se sw -> drawQuadtree nw ++ drawQuadtree ne ++ drawQuadtree se ++ drawQuadtree sw

drawMany : A.Array (Positioned a) -> List (Form)
drawMany ar = A.toList <| A.map draw ar

draw : Positioned a -> Form
draw a =
    move (a.x,a.y) (filled blue (rect 20 20))
