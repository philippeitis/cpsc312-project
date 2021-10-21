module Main where

import Lib
import Tokenizer(tokenize, setupTokenizer)

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "programz 4 kidz" (500, 500) (24, 24)

background :: Color
background = white

data Universe = Empty Float

render :: Universe -> Picture
render (Empty radius) = circle radius

update :: ViewPort -> Float -> Universe -> Universe
update _ _ (Empty n) = if n < 80 then Empty (n + 1) else Empty 40


main :: IO ()
main = setupTokenizer >> tokenize "Hello world"
    >>= print >> simulate window background 60 (Empty 40) render update