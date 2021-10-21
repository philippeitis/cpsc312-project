module NLTree (
    parse,
    NLTree(..),
    Object(..),
    Verb(..),
    Adjective(..),
    Noun(..)
) where

import Tokens (Tag (..), Token (..))

data Adjective = Adjective Tag String
data Noun = Noun Tag String

data Object = Object [Adjective] Noun

data Verb = Verb Tag String

data NLTree = VerbObject Verb Object

parse :: [Token] -> Maybe NLTree
parse [Token VB vb, Token DT _, Token JJ adj, Token NN noun] = Just (VerbObject
        (Verb VB vb)
        (Object [Adjective JJ adj] (Noun NN noun))
    )
parse _ = Nothing
