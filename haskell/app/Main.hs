module Main where

import Lib
import Tokenizer(tokenize, setupTokenizer)

import Graphics.Gloss

window :: Display
window = InWindow "programz 4 kidz" (500, 500) (24, 24)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = setupTokenizer >> tokenize "Hello world"
    >>= print >> display window background drawing