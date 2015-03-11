module Game.Main where

import Game.Model (..)
import Game.Controller (..)
import Game.View (..)
import Game.Quadtree (..)
import Array as A
import Graphics.Element (Element)
import Window
import Signal (..)


foo1 = {x=100,y=1}

foo2 = {x=12,y=-5}

foo3 = {x=-50,y=5}

bounds : AABB
bounds = {center={x=0,y=0},dimensions={width=100,height=100}}

foos = A.push foo3 <| A.push foo2 <| A.push foo1 A.empty

quadt : Quadtree (Positioned {})
quadt = insertMany foos (emptyQT bounds 5)

quadt2 = updateSingle (\f -> {f | x <- -100 , y <- -1}) foo1 quadt

quadSig : Signal (Quadtree (Positioned {}))
quadSig =
    constant quadt2



main : Signal Element
main =
    view <~ Window.dimensions ~ stateSig
