module Game.Controller where

import Game.Model (..)
import Game.Quadtree (..)
import Array as A
import Time (..)
import Keyboard
import Signal
import Char

upVelocity : Bool -> { x:Int, y:Int } -> CharModel -> CharModel
upVelocity isRunning {x,y} m =
    let scale = if isRunning then 2 else 1 in
    { m |
        dx <- scale * toFloat x,
        dy <- scale * toFloat y
    }

upPosition : Time -> CharModel -> CharModel
upPosition dt ({x,y,dx,dy,atks,dir,col} as m) =
    { m |
        x <- (x + dt * dx),
        y <- (y + dt * dy)
    }

newAtk : Time -> Bool -> AttackState -> AttackState
newAtk dt isAttacking a = case a of
    CanAtk    ->     if isAttacking then Atk atkTime else CanAtk
    Atk t     ->     let newT = t - dt in
                        if newT <= 0
                            then CanAtk
                            else Atk newT

upAtkState : Time -> Bool -> CharModel -> CharModel
upAtkState dt isAttacking ({x,y,dx,dy,atks,dir,col} as m) =
    { m |
        atks <- newAtk dt isAttacking atks
    }

upDir : { x:Int, y:Int} -> CharModel -> CharModel
upDir {x,y} ({x,y,dx,dy,atks,dir,col} as m) =
    { m |
        dir <- if | x < 0 -> Left
                     | x > 0 -> Right
                     | y < 0 -> Down
                     | y > 0 -> Up
                     | otherwise -> m.dir
    }

-- NOTE(jsalem): Keyboard.arrows return signal {x:Int, y:Int}
upCharModel : (Time, { x:Int, y:Int }, Bool, Bool) -> CharModel -> CharModel
upCharModel (dt, dir, isRunning, isAttacking) m =
    m
    |> upVelocity isRunning dir
    |> upPosition dt
    |> upAtkState dt isAttacking
    |> upDir dir

-- Signals

localEntities : Signal (Positioned a) -> Signal (Quadtree (Positioned a)) -> Signal (A.Array (Positioned a))
localEntities a qt =
    Signal.map2 (\a q -> queryPoint {x=a.x,y=a.y} q) a qt

playerSig : Signal CharModel
playerSig =
    Signal.foldp upCharModel player input

deltaTime : Signal Time
deltaTime = (fps 60) -- gotta have that 60 fps

input : Signal (Time, { x:Int, y:Int }, Bool, Bool)
input =
    Signal.sampleOn deltaTime
        (Signal.map4 (,,,)
            deltaTime
            Keyboard.arrows
            Keyboard.shift
            {-(Keyboard.isDown (Char.toCode 'c'))-}
            Keyboard.space)
