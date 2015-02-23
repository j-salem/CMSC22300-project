module Game.Main where

import Game.Model (..)
import Game.Controller (..)
import Game.View (..)
import Graphics.Element (Element)
import Window
import Signal (..)

main : Signal Element
main =
    view <~ Window.dimensions ~ playerSig
