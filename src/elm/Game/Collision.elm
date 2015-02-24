module Game.Collision where


--type Actor = Player | Weapon | Enemy

-- NOTE(jsalem): The anchor of the Rect is the center so be sure to check edges
--               when doing collisions
type alias Rect = { w:Float, h:Float }

rightEdge : {x:Float,y:Float} -> Rect -> Float
rightEdge {x,y} {w,h} = x + (w / 2)

leftEdge : {x:Float,y:Float} -> Rect -> Float
leftEdge {x,y} {w,h} = x - (w / 2)

topEdge : {x:Float,y:Float} -> Rect -> Float
topEdge {x,y} {w,h} = y + (h / 2)

bottomEdge : {x:Float,y:Float} -> Rect -> Float
bottomEdge {x,y} {w,h} = y - (h / 2)

isCollision : {x:Float,y:Float} -> Rect -> {x:Float,y:Float} -> Rect -> Bool
isCollision pos1 dim1 pos2 dim2 =
    if  | (rightEdge pos1 dim1) < (leftEdge pos2 dim2) -> False
        | (leftEdge pos1 dim1) > (rightEdge pos2 dim2) -> False
        | (topEdge pos1 dim1) < (bottomEdge pos2 dim2) -> False
        | (bottomEdge pos1 dim1) > (topEdge pos2 dim2) -> False
        | otherwise -> True
