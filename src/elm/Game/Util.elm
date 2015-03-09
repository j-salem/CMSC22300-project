module Game.Util where

lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp y0 y1 x0 x x1 =
    y0 + (y1 - y0) * ((x - x0) / (x1 - x0))
