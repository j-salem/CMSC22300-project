module Game.Controller where

import Game.Model (..)
import Game.Quadtree (..)
import Game.Collision (..)
import Array as A
import Time (..)
import Keyboard
import Signal
import Char


----------------------------
----- Update Functions -----
----------------------------
-- checks for collisions as a result of move
-- TODO(jsalem): make pretty, cause this... this is ugly
checkMove : (Float,Float) -> Rect -> A.Array (Positioned (Bounded a)) -> Collision
checkMove (x,y) col ar =
    let colAr = A.map (\b -> isCollisionDetailed {x=x,y=y} col {x=b.x,y=b.y} b.col) ar
        colArLen = A.length (A.filter (\i -> i /= NoCol) colAr)
        colArFilterLen = A.length (A.filter (\i -> i == ColX) colAr)
    in
    if  | A.length ar == 0        -> NoCol
        | colArLen == 0              -> NoCol
        | colArFilterLen == 0        -> ColY
        | colArFilterLen == colArLen -> ColX
        | otherwise                  -> ColXY

upVelocity : Bool -> { x:Int, y:Int } -> CharModel -> CharModel
upVelocity isRunning {x,y} m =
    let scale = if isRunning then 2 else 1 in
    { m |
        dx <- scale * toFloat x,
        dy <- scale * toFloat y
    }

upPosition : Time -> A.Array (Positioned (Bounded a)) -> CharModel -> CharModel
upPosition dt ar ({x,y,dx,dy,atks,dir,col} as m) =
    let newX = x + dt * dx
        newY = y + dt * dy
        collisions = checkMove (newX,newY) col ar
        upX = if (collisions == ColX  || collisions == ColXY) then x else newX
        upY = if (collisions == ColY  || collisions == ColXY) then y else newY
    in
    { m |
        x <- upX,
        y <- upY
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
upCharModel : (Time, { x:Int, y:Int }, Bool, Bool) -> A.Array (Positioned (Bounded a)) -> CharModel -> CharModel
upCharModel (dt, dir, isRunning, isAttacking) ar m =
    m
    |> upVelocity isRunning dir
    |> upPosition dt ar
    |> upAtkState dt isAttacking
    |> upDir dir

upState : (Time, { x:Int, y:Int }, Bool, Bool) -> GameState -> GameState
upState inputs ({player,currentRoom} as gs) =
    let nearby = queryPoint {x=player.x,y=player.y} currentRoom.contents in
    { gs |
        player <- (upCharModel inputs nearby player)
    }

-- Signals

-- updateSingle : (Positioned a -> Positioned a) -> Positioned a -> Quadtree (Positioned a) -> Quadtree (Positioned a)

localEntities : Signal (Positioned a) -> Signal (Quadtree (Positioned a)) -> Signal (A.Array (Positioned a))
localEntities a qt =
    Signal.map2 (\a q -> queryPoint {x=a.x,y=a.y} q) a qt

-- playerSig : Signal CharModel
-- playerSig =
--     Signal.foldp upCharModel input state

stateSig : Signal GameState
stateSig =
    Signal.foldp upState state input

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
