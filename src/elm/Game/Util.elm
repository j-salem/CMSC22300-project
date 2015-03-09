module Game.Util where

import Maybe (..)
import Array as A
import Trampoline (..)

lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp y0 y1 x0 x x1 =
    y0 + (y1 - y0) * ((x - x0) / (x1 - x0))

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


fromJust : Maybe a -> a
fromJust (Just a) = a
