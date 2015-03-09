module Game.Model where

import Game.Collision (..)
import Game.Quadtree (..)
import Array as A

------------------------------
------ Game State Model ------
------------------------------

type alias GameState =
    { player:CharModel, currentRoom:Room }


------------------------------
--------- Room Model ---------
------------------------------

type alias Room =
    { id:Int, contents:Quadtree (NonPlayerPositioned) }

------------------------------
-------- Entity Model --------
------------------------------

type alias NonPlayerPositioned = Positioned ({foo:Float, bar:Float})

------------------------------
-------- Player Model --------
------------------------------

-- Positioned -> CharModel  (think subcalss)
-- contains pos (x,y) and velocities (dx,dy)
type alias CharModel =
  { x:Float, y:Float, dx:Float, dy:Float, atks:AttackState, dir:Dir, col:Rect }

-- Our hero
player : CharModel
player =
    { x = 0, y = 0, dx = 0, dy = 0, atks = CanAtk, dir = Down, col = {width=50,height=30}}

-- States
-- NOTE(jsalem): Is either in that state with timer or is able to
atkTime : Float
atkTime = 500.0
type AttackState = CanAtk | Atk Float
type Dir = Up | Down | Right | Left
