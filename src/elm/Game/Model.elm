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

-- Test Room with test objects
type alias Foo = Blockable (Positioned (Bounded {}))
sqr : Rect
sqr = {width = 30, height = 30}

foo1 = {x=100,y=100, col = sqr, isBlocking=True }

foo2 = {x=100,y=-75, col = sqr, isBlocking=True }

foo3 = {x=-81,y=40, col = sqr, isBlocking=True }

foos = A.push foo3 <| A.push foo2 <| A.push foo1 A.empty

qt : Quadtree (Blockable (Positioned (Bounded {})))
qt = insertMany foos (emptyQT {center={x=0,y=0}, dimensions={width=800,height=600}} 5)

arena : Room
arena = { id=0, contents = qt }

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
