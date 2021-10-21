module Main where

import Lib
import Tokenizer(tokenize, setupTokenizer)
import Interpret(apply)
import NLTree(parse)
import World(defaultWorld, worldToPicture, updateWorld, World(..))
import Graphics.Gloss
import Data.Maybe(maybe)
import Graphics.Gloss.Interface.IO.Game(playIO, SpecialKey(..), Key(..), Event(..), KeyState(..))

window :: Display
window = InWindow "programz 4 kidz" (500, 500) (24, 24)

background :: Color
background = white

maybeOr :: Maybe a -> a -> a 
maybeOr Nothing val = val
maybeOr (Just val) _ = val

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (Char c) Up _ _) (World items s) = return (World items (s ++ [c]))
handleEvent (EventKey (SpecialKey KeyEnter) Up _ _) (World items s) = tokenize s 
    >>= \tokens -> return (maybeOr (
        tokens >>= parse >>= \tree -> Just (apply tree newWorld)
        ) newWorld)
    where newWorld = World items ""
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) (World items s) = return (World items (s ++ " "))

handleEvent event world = return world

main :: IO ()
main = setupTokenizer
    >>= \tokens -> playIO
        window
        background
        60
        defaultWorld
        worldToPicture
        handleEvent
        updateWorld
