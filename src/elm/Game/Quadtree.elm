module Game.Quadtree where

import Game.Collision (..)
import Game.Util (fromJust)
import Trampoline (..)
import Array as A

-- http://en.wikipedia.org/wiki/Quadtree

-- Axis Aligned Bounding Box with half dimensions (c -> center, d->dimensions)
type alias AABB = { center:XY, dimensions:Rect }

type alias Positioned a = { a | x:Float, y:Float }

-- Quadtree containing type a is either:
-- > Leaf with bounding box, max number of elements, and array containing elems
-- > Node with bounding box, nw, ne, se, sw quadrants represented as Quadtrees
--
-- Notes: Traverse quadrants in clockwise order starting in NW

type Quadtree a =
    Leaf AABB Int (A.Array a) |
    Node AABB (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a)

emptyQT : AABB -> Int -> Quadtree a
emptyQT aabb maxElem =
    Leaf aabb maxElem A.empty

aabbQT : Quadtree a -> AABB
aabbQT qt = case qt of
    Leaf aabb _ _     -> aabb
    Node aabb _ _ _ _ -> aabb

maxElemQT : Quadtree a -> Int
maxElemQT qt = case qt of
    Leaf _ me _       -> me
    Node _ nw _ _ _    -> maxElemQT nw

subdivideAABB : AABB -> (AABB, AABB, AABB, AABB)
subdivideAABB {center,dimensions} =
    let
        nwXY = {x = center.x - dimensions.width / 2, y = center.y + dimensions.height / 2}
        neXY = {x = center.x + dimensions.width / 2, y = center.y + dimensions.height / 2}
        seXY = {x = center.x + dimensions.width / 2, y = center.y - dimensions.height / 2}
        swXY = {x = center.x - dimensions.width / 2, y = center.y - dimensions.height / 2}
        newRect = {width = dimensions.width / 2, height = dimensions.height / 2}
    in
    ({center = nwXY, dimensions = newRect},
     {center = neXY, dimensions = newRect},
     {center = seXY, dimensions = newRect},
     {center = swXY, dimensions = newRect})

subdivide : Quadtree (Positioned a) -> Quadtree (Positioned a)
subdivide (Leaf aabb maxElem ar) =
    let (nw,ne,se,sw) = subdivideAABB aabb in
    insertMany ar
    <| (Node aabb
            (emptyQT nw maxElem)
            (emptyQT ne maxElem)
            (emptyQT se maxElem)
            (emptyQT sw maxElem))

-- PRE: type a is required to have an x and y
insert : Positioned a -> Quadtree (Positioned a) -> Quadtree (Positioned a)
insert elem qt = case qt of
    Node aabb nw ne se sw ->
        if | containsPointQT nw {x=elem.x,y=elem.y} -> Node aabb (insert elem nw) ne se sw
           | containsPointQT ne {x=elem.x,y=elem.y} -> Node aabb nw (insert elem ne) se sw
           | containsPointQT se {x=elem.x,y=elem.y} -> Node aabb nw ne (insert elem se) sw
           | otherwise                              -> Node aabb nw ne se (insert elem sw)
    Leaf aabb maxElem ar ->
        if | (A.length ar) >= maxElem -> subdivide (Leaf aabb maxElem (A.push elem ar))
           | otherwise                -> Leaf aabb maxElem (A.push elem ar)


forEach : A.Array a -> b -> (a -> b -> b) -> b
forEach a b f =
    let
        isDone arr = case A.get 0 arr of
            Nothing -> True
            _       -> False
        foo pred f ar acc = case pred ar of
            True  -> Done (acc)
            False -> Continue (\() -> foo pred f (A.slice 1 (A.length ar) ar) (f (fromJust (A.get 0 ar)) acc))
    in
        trampoline <| foo isDone f a b

-- Potentially won't work
insertMany : A.Array (Positioned a) -> Quadtree (Positioned a) -> Quadtree (Positioned a)
insertMany a qt =
    forEach a qt insert

removeA : a -> A.Array a -> A.Array a
removeA a ar = A.filter (\i -> i /= a) ar

remove : Positioned a -> Quadtree (Positioned a) -> Quadtree (Positioned a)
remove a qt = case qt of
    Node aabb nw ne se sw ->
        if  | containsPointQT nw {x = a.x, y = a.y} -> remove a nw
            | containsPointQT ne {x = a.x, y = a.y} -> remove a ne
            | containsPointQT se {x = a.x, y = a.y} -> remove a se
            | otherwise                    -> remove a sw
    Leaf aabb maxElem ar -> Leaf aabb maxElem (removeA a ar)

queryPoint: {x:Float, y:Float} -> Quadtree (Positioned a) -> A.Array (Positioned a)
queryPoint ({x,y} as p) qt = case qt of
    Node aabb nw ne se sw ->
        if  | containsPointQT nw {x = p.x, y = p.y} -> queryPoint p nw
            | containsPointQT ne {x = p.x, y = p.y} -> queryPoint p ne
            | containsPointQT se {x = p.x, y = p.y} -> queryPoint p se
            | otherwise                    -> queryPoint p sw
    Leaf aabb maxElem ar -> ar

containsPoint : AABB -> { x:Float, y:Float } -> Bool
containsPoint {center,dimensions} {x,y} =
    if  (x < center.x + dimensions.width / 2) &&
        (x > center.x - dimensions.width / 2) &&
        (y < center.y + dimensions.height / 2) &&
        (y > center.y - dimensions.height / 2)
    then True else False

containsPointQT : Quadtree (Positioned a) -> { x:Float, y:Float} -> Bool
containsPointQT qt xy =
    if containsPoint (aabbQT qt) xy then True else False

intersectsAABB : AABB -> AABB -> Bool
intersectsAABB a b =
    isCollision a.center a.dimensions b.center b.dimensions

allItems : Quadtree (Positioned a) -> A.Array (Positioned a)
allItems qt = case qt of
    Node aabb nw ne se sw -> A.append (allItems nw) <| A.append (allItems ne) <| A.append (allItems se) (allItems sw)
    Leaf aabb maxElem ar -> ar

fixQT : Quadtree (Positioned a) -> Quadtree (Positioned a)
fixQT qt =
    insertMany (allItems qt)
    <| emptyQT (aabbQT qt) (maxElemQT qt)
