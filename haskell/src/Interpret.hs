module Interpret (apply) where

import Data.Maybe (mapMaybe)
import Graphics.Gloss
import NLTree
import Parse
import Tokens (Tag (..), Token (..))
import World

-- |Interprets the tree and applies the interpreted effect to the world, doing nothing if the effect is unrecognized
apply :: NLTree -> World -> World
apply (VerbObject (Verb VB "draw") (Object adjs (Noun NN "cat"))) (World items s) =
    World (Individual (mapMaybe parseAdjective adjs) Cat Center : items) s
apply _ world = world
