module Game.Quadtree where

import Game.Collision (..)
import Trampoline (..)
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

maxElemQT : Quadtree a -> Int
maxElemQT qt = case qt of
    Leaf _ me _       -> me
    Node _ me _ _ _ _ -> me

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
<<<<<<< HEAD
        if | {- Check if in Array -}     -> Leaf aabb maxElem ar
           | (A.length ar) + 1 > maxElem -> subdivide (Leaf aabb maxElem (A.push elem ar))
           | otherwise                   -> Leaf aabb maxElem (A.push elem ar)
=======
        if | (A.length ar) >= maxElem -> subdivide (Leaf aabb maxElem (A.push elem ar))
           | otherwise                -> Leaf aabb maxElem (A.push elem ar)


forEach : A.Array a -> b -> (a -> b -> b) -> b
forEach a b f =
    let
        isDone arr = if A.get arr 0 == Nothing then True else False
        foo pred f {ar,acc} = case pred acc of
            True  -> Done (acc)
            False -> Continue (foo pred f {ar = A.slice 1 (A.length ar), acc = f (A.get 0 ar) acc})
    in
        trampoline <| foo isDone f {a,b}

-- Potentially won't work
insertMany : A.Array Entity a -> Quadtree Entity a -> Quadtree Entity a
insertMany a qt =
    forEach a qt insert

removeA : a -> A.Array a -> A.Array a
removeA a ar = A.filter (\i -> i /= a) ar

remove : Entity a -> Quadtree Entity a -> Quadtree Entity a
remove a qt = case qt of
    Node aabb nw ne se sw ->
        if  | containsPointQT nw {a.x,a.y} -> remove a nw
            | containsPointQT ne {a.x,a.y} -> remove a ne
            | containsPointQT se {a.x,a.y} -> remove a se
            | otherwise                    -> remove a sw
    Leaf aabb maxElem ar -> Leaf aabb maxElem (removeA a ar)

queryPoint: {x:Float, y:Float} -> Quadtree Entity a -> A.Array Entity a
queryPoint ({x,y} as p) qt = case qt of
    Node aabb nw ne se sw ->
        if  | containsPointQT nw {a.x,a.y} -> queryPoint p nw
            | containsPointQT ne {a.x,a.y} -> queryPoint p ne
            | containsPointQT se {a.x,a.y} -> queryPoint p se
            | otherwise                    -> queryPoint p sw
    Leaf aabb maxElem ar -> ar

>>>>>>> 645e561... more qt functionality and removed unusedvectormath

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

allItems : Quadtree a -> A.Array a
allItems qt = case qt of
    Node aabb nw ne se sw -> A.append (allItems nw) <| A.append (allItems ne) <| A.append (allItems se) (allItems sw)
    Leaf aabb maxElem ar -> ar

fixQT : Quadtree a -> Quadtree a
fixQT qt =
    insertMany (allItems qt)
    <| emptyQT (aabbQT qt) (maxElemQT qt)
