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
  deriving(Show, Eq)

data Noun = Noun Tag String
  deriving(Show, Eq)

data Object = Object [Adjective] Noun
  deriving(Show, Eq)

data Verb = Verb Tag String
  deriving(Show, Eq)

data NLTree = VerbObject Verb Object
    deriving(Show, Eq)

-- |Parses the token stream into a tree
parse :: [Token] -> Maybe NLTree
parse [Token VB vb, Token DT _, Token NN noun] = Just (VerbObject
        (Verb VB vb)
        (Object [] (Noun NN noun))
    )
parse [Token VB vb, Token DT _, Token JJ adj, Token NN noun] = Just (VerbObject
        (Verb VB vb)
        (Object [Adjective JJ adj] (Noun NN noun))
    )
parse _ = Nothing
