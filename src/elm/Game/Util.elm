module Game.Util where

lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp y0 y1 x0 x x1 =
    y0 + (y1 - y0) * ((x - x0) / (x1 - x0))

type alias Vec2 = { x:Float, y:Float }

dotProd : Vec2 -> Vec2 -> Float
dotProd {x1,y1} {x2,y2} =
    (x1 * x2) + (y1 * y2)

scaleVec : Float -> Vec2 -> Vec2
scaleVec s {x,y} = { x = s * x, y = s * y }

scaleVecX : Float -> Vec2 -> Vec2
scaleVecX s {x,y} = { x = s*x, y = y }

scaleVecY : Float -> Vec2 -> Vec2
scaleVecY s {x,y} = { x = x, y = y*s }

addVec : Vec2 -> Vec2 -> Vec2
addVec {x1,y1} {x2,y2} = { x = x1+x2, y = y1+y2 }

subVec : Vec2 -> Vec2 -> Vec2
subVec {x1,y1} {x2,y2} = { x = x1-x2, y = y1-y2 }
