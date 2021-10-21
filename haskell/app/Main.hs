module Main where

import Lib
import Tokenizer(tokenize, setupTokenizer)
import Interpret(apply)
import NLTree(parse)
import World(defaultWorld, worldToPicture, updateWorld)
import Graphics.Gloss
import Data.Maybe(maybe)

window :: Display
window = InWindow "programz 4 kidz" (500, 500) (24, 24)

background :: Color
background = white

maybeOr :: Maybe a -> a -> a 
maybeOr Nothing val = val
maybeOr (Just val) _ = val

main :: IO ()
main = setupTokenizer
    >> tokenize "draw a black cat"
    >>= \tokens -> simulate
        window
        background
        60
        (maybeOr (tokens >>= parse >>= \tree -> Just (apply tree defaultWorld)) defaultWorld)
        worldToPicture
        updateWorld
