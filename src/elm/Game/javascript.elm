import Http
import Json.Decode (..)
import Json.Encode
import Signal ((<~),(~))
import Signal
import List (..)
import Graphics.Element
import Graphics.Collage (..)
import Text
import Basics (..)
import Color (..)
import Mouse

type alias Room = {level:List Int, width:Int, height:Int}


sampleListOn : Signal.Signal b -> List a -> Signal.Signal a
sampleListOn ticker xs = 
  let cycler _ ts = tail ts ++ [head ts]
  in Signal.map head (Signal.foldp cycler xs (ticker))

serverJson : String -> Signal.Signal Graphics.Element.Element
serverJson i = let jsonTxtSig = respStr <~ (Http.sendGet (Signal.constant ("./level" ++ i ++".json")))
             in toMap <~ (getJsonArray <~ jsonTxtSig)
             
signalRoom : Signal.Channel String
signalRoom =
  Signal.channel "1"

getJsonArray : String -> Room
getJsonArray jsonTxt = case decodeString (object3 Room ("level" := list int) ("width" := int) ("height" := int)) jsonTxt of
                        Ok x -> x
                        _ -> {level=[],width=0,height=0}

respStr : Http.Response String -> String
respStr resp = case resp of
    Http.Success string -> string
    Http.Waiting -> "[1,2]"
    _ -> "[1]"

showResults : Room -> Graphics.Element.Element
showResults json = Graphics.Element.flow Graphics.Element.down [Text.asText json]

toMap : Room -> Graphics.Element.Element
toMap room = collage 600 600 (parseLevel room.level room.width room.height (-1,0))

--Justin: This needs error validation for malformed levels at some point
parseLevel : List Int -> Int -> Int -> (Int,Int) -> List Form
parseLevel lvl w h (x,y)  = let newIndex = if | x == w-1 -> (0,y+1)
                                              | otherwise -> (x+1,y)
  in case lvl of
    i :: is -> if | i == 0 -> (transformShape (filled red (square 20.0)) newIndex) :: parseLevel is w h newIndex
                  | i == 1 -> (transformShape (filled blue (square 20.0)) newIndex) :: parseLevel is w h newIndex
                  | i == 2 -> (transformShape (filled green (square 20.0)) newIndex) :: parseLevel is w h newIndex
    [] -> []
  
transformShape : Form -> (Float,Float) -> Form
transformShape shape (x,y) = move (20*x,-20*y) shape

main = serverJson "2"