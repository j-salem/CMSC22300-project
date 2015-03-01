module Game.Quadtree where

import Game.Collision (..)
import Array as A

-- http://en.wikipedia.org/wiki/Quadtree

-- 2point
type alias XY = { x:Float, y:Float }
-- Axis Aligned Bounding Box with half dimensions
type alias AABB = { center:XY, dim:Rect }

-- Quadtree containing type a is either:
-- > Leaf with bounding box, max number of elements, and array containing elems
-- > Node with bounding box, nw, ne, se, sw quadrants represented as Quadtrees
--
-- Notes: Traverse quadrants in clockwise order starting in NW

-- NEED ELM EXTENSIBLE RECORDS
type Quadtree a =
    Leaf AABB Int (A.Array a) |
    Node AABB (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a)

emptyQT : AABB -> Int -> Quadtree
emptyQT aabb maxElem =
    Leaf aabb maxElem A.empty

aabbQT : Quadtree a -> AABB
aabbQT qt = case qt of
    Leaf aabb _ _     -> aabb
    Node aabb _ _ _ _ -> aabb

subdivideAABB : AABB -> (AABB, AABB, AABB, AABB)
subdivideAABB {{x,y},{w,h}} =
    let
        nwXY = {x = x - w / 2, y = y + h / 2}
        neXY = {x = x + w / 2, y = y + h / 2}
        seXY = {x = x + w / 2, y = y - h / 2}
        swXY = {x = x - w / 2, y = y - h / 2}
        newRect = {w = w / 2, h = h / 2}
    in
    ({center = nwXY, dim = newRect},
     {center = neXY, dim = newRect},
     {center = seXY, dim = newRect},
     {center = swXY, dim = newRect})

subdivide : Quadtree a -> Quadtree a
subdivide (Leaf aabb maxElem ar) =
    let (nw,ne,se,sw) = subdivideAABB aabb in
    insertMany ar
    <| (Node aabb
            (emptyQT nw maxElem)
            (emptyQT ne maxElem)
            (emptyQT se maxElem)
            (emptyQT sw maxElem))

-- PRE: type a is required to have an x and y
insert : a -> Quadtree a -> Quadtree a
insert elem qt = case qt of
    Node aabb nw ne se sw ->
        if | containsPointQT nw {elem.x,elem.y} -> Node aabb (insert elem nw) ne se sw
           | containsPointQT ne {elem.x,elem.y} -> Node aabb nw (insert elem ne) se sw
           | containsPointQT se {elem.x,elem.y} -> Node aabb nw ne (insert elem se) sw
           | otherwise                          -> Node aabb nw ne se (insert elem sw)
    Leaf aabb maxElem ar ->
        if | {- Check if in Array -}     -> Leaf aabb maxElem ar
           | (A.length ar) + 1 > maxElem -> subdivide (Leaf aabb maxElem (A.push elem ar))
           | otherwise                   -> Leaf aabb maxElem (A.push elem ar)

containsPoint : AABB -> { x:Float, y:Float } -> Bool
containsPoint {{cx,cy},{w,h}} {x,y} =
    if  x < cx + w / 2 and
        x > cx - w / 2 and
        y < cy + h / 2 and
        y > cy - h / 2
    then True else False

containsPointQT : Quadtree a -> { x:Float, y:Float} -> Bool
containsPointQT qt xy =
    if containsPoint (aabbQT qt) xy then True else False

intersectsAABB : AABB -> AABB -> Bool
intersectsAABB {c1,d1} {c2,d2} =
    isCollision c1 d1 c2 d2
