module Parse where

import Graphics.Gloss
import NLTree
import Tokens (Tag (..), Token (..))
import World (Attribute (..), Size (..))

data Action = Draw

-- |Parses the action to a supported command, or returns Nothing if the action
-- is not supported.
parseAction :: Verb -> Maybe Action
parseAction (Verb VB "draw") = Just Draw
parseAction _ = Nothing

-- |Parses the adjective to an Attribute that can be applied to an individual,
--  or returns Nothing if the adjective is not supported.
parseAdjective :: Adjective -> Maybe Attribute
parseAdjective (Adjective JJ "black") = Just (AColor black)
parseAdjective (Adjective JJ "red") = Just (AColor red)
parseAdjective (Adjective JJ "blue") = Just (AColor blue)
parseAdjective (Adjective JJ "green") = Just (AColor green)
parseAdjective (Adjective JJ "orange") = Just (AColor orange)
parseAdjective (Adjective JJ "big") = Just (Size Big)
parseAdjective (Adjective JJ "small") = Just (Size Small)
parseAdjective _ = Nothing
