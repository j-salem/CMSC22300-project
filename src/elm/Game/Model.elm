module Game.Model where

-- contains pos (x,y) and velocities (dx,dy)
type alias CharModel =
  { x:Float, y:Float, dx:Float, dy:Float }

-- Our hero
player : CharModel
player =
    { x = 0, y = 0, dx = 0, dy = 0 }


-- States

type JumpState = CanJump | Jumping
type AttackState = CanAtk | Attacking
