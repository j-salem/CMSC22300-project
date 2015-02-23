module Game.Model where

-- contains pos (x,y) and velocities (dx,dy)
type alias CharModel =
  { x:Float, y:Float, dx:Float, dy:Float, atks:AttackState, dir:Dir }

type Dir = Up | Down | Right | Left

-- Our hero
player : CharModel
player =
    { x = 0, y = 0, dx = 0, dy = 0, atks = CanAtk, dir = Down }


-- States

-- NOTE(jsalem): Is either in that state with timer or is able to
atkTime : Float
atkTime = 500.0
type AttackState = CanAtk | Atk Float
