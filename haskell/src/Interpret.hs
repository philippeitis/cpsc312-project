module Interpret (apply) where

import Graphics.Gloss
import World
import NLTree
import Tokens(Token(..), Tag(..))
import Data.Maybe(mapMaybe)

data Action = Draw

parseAction :: Verb -> Maybe Action
parseAction (Verb VB "draw") = Just Draw
parseAction _ = Nothing

parseAdjective :: Adjective -> Maybe Attribute
parseAdjective (Adjective JJ "black") = Just (AColor black)

apply :: NLTree -> World -> World
apply (VerbObject (Verb VB "draw") (Object adjs (Noun NN "cat"))) (World items s) =
    World (Individual (mapMaybe parseAdjective adjs) Cat Center : items) s
apply _ world = world