module Game.Controller where

import Game.Model (..)
import Time (..)
import Keyboard
import Signal

upVelocity : Bool -> { x:Int, y:Int } -> CharModel -> CharModel
upVelocity isRunning {x,y} m =
    let scale = if isRunning then 2 else 1 in
    { m |
        dx <- scale * toFloat x,
        dy <- scale * toFloat y
    }

upPosition : Time -> CharModel -> CharModel
upPosition dt ({x,y,dx,dy} as m) =
    { m |
        x <- (x + dt * dx),
        y <- (y + dt * dy)
    }

-- NOTE(jsalem): Keyboard.arrows return signal {x:Int, y:Int}
upCharModel : (Time, { x:Int, y:Int }, Bool) -> CharModel -> CharModel
upCharModel (dt, dir, isRunning) m =
    m |> upVelocity isRunning dir |> upPosition dt

-- Signals

playerSig : Signal CharModel
playerSig =
    Signal.foldp upCharModel player input

deltaTime : Signal Time
deltaTime = (fps 60) -- gotta have that 60 fps

input : Signal (Time, { x:Int, y:Int }, Bool)
input =
    Signal.sampleOn deltaTime (Signal.map3 (,,) deltaTime Keyboard.arrows Keyboard.shift)
