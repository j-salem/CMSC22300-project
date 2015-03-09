module Game.Collision where

--type Actor = Player | Weapon | Enemy

-- NOTE(jsalem): The anchor of the Rect is the center so be sure to check edges
--               when doing collisions
type alias Rect = { width:Float, height:Float }

rightEdge : {x:Float,y:Float} -> Rect -> Float
rightEdge {x,y} {width,height} = x + (width / 2)

leftEdge : {x:Float,y:Float} -> Rect -> Float
leftEdge {x,y} {width,height} = x - (width / 2)

topEdge : {x:Float,y:Float} -> Rect -> Float
topEdge {x,y} {width,height} = y + (height / 2)

bottomEdge : {x:Float,y:Float} -> Rect -> Float
bottomEdge {x,y} {width,height} = y - (height / 2)

-- Base level collision checking.
isCollision : {x:Float,y:Float} -> Rect -> {x:Float,y:Float} -> Rect -> Bool
isCollision pos1 dim1 pos2 dim2 =
    if  | (rightEdge pos1 dim1) < (leftEdge pos2 dim2) -> False
        | (leftEdge pos1 dim1) > (rightEdge pos2 dim2) -> False
        | (topEdge pos1 dim1) < (bottomEdge pos2 dim2) -> False
        | (bottomEdge pos1 dim1) > (topEdge pos2 dim2) -> False
        | otherwise -> True
