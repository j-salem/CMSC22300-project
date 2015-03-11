module Game.Model where

import Game.Collision (..)
import Game.Quadtree (..)
import Array as A

------------------------------
------ Game State Model ------
------------------------------

type alias GameState =
    { player:CharModel, currentRoom:Room }
state : GameState
state = { player = player, currentRoom = arena }

------------------------------
--------- Room Model ---------
------------------------------

type alias Room =
    { id:Int, contents:Quadtree (Blockable (Positioned (Bounded {}))) }

arena : Room
arena = { id=0, contents = (emptyQT {center={x=0,y=0}, dimensions={width=800,height=600}} 5) }

------------------------------
-------- Entity Model --------
------------------------------
-- type alias Positioned a = { a | x:Float, y:Float }  -- in Quadtree.elm
type alias Blockable a = { a | isBlocking:Bool }
type alias Moving a = { a | dx:Float, dy:Float }
type alias Bounded a = { a | col:Rect } -- col for collision

------------------------------
-------- Player Model --------
------------------------------

-- Positioned -> CharModel  (think subcalss)
-- contains pos (x,y) and velocities (dx,dy)
type alias CharModel =
  Positioned ( Moving ( Bounded ({ atks:AttackState, dir:Dir })))

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
