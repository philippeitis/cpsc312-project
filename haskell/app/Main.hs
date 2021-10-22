module Main where

import Data.Maybe (maybe)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..), playIO)
import Interpret (apply)
import NLTree (parse)
import Tokenizer (setupTokenizer, tokenize)
import World (World (..), defaultWorld, updateWorld, worldToPicture)

-- |Creates the initial window.
window :: Display
window = InWindow "C is for Coding" (500, 500) (24, 24)

background :: Color
background = white

-- |Utility function to return the item inside the maybe, or a default value.
maybeOr :: Maybe a -> a -> a
maybeOr Nothing val = val
maybeOr (Just val) _ = val

-- |Utility function to produce a list with all but the last item
popLast :: [a] -> [a]
popLast [] = []
popLast items = init items

-- |Detects user input and collects it into a sentence that will be tokenized and parsed
--  If the user hits Enter, the input so far is interpreted and the world is modified.
--  All other inputs are ignored.
handleEvent :: Event -> World -> IO World
-- We specifically check for up (to show that key is released)
-- Character input
handleEvent (EventKey (Char c) Up _ _) (World items s) = return (World items (s ++ [c]))
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) (World items s) = return (World items (s ++ " "))
-- Enter
handleEvent (EventKey (SpecialKey KeyEnter) Up _ _) (World items s) = tokenize s
    >>= \tokens -> return (maybeOr (
        tokens >>= parse >>= \tree -> Just (apply tree newWorld)
        ) newWorld)
    where newWorld = World items ""
-- Backspace, delete
handleEvent (EventKey (SpecialKey KeyBackspace) Up _ _) (World items s) = putStrLn "del"
  >> putStrLn (popLast s)
  >> return (World items (popLast s))
handleEvent (EventKey (SpecialKey KeyDelete) Up _ _) (World items s) = putStrLn "del"
  >> putStrLn (popLast s)
  >> return (World items (popLast s))
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
