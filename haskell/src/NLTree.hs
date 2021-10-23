module NLTree (
    parse,
    NLTree(..),
    Object(..),
    Verb(..),
    Adjective(..),
    Noun(..)
) where

import Tokens (Tag (..), Token (..))

-- |An adjective describes an object, and includes the JJ, JJR, JJS Tags, and the underlying text.
data Adjective = Adjective Tag String
  deriving(Show, Eq)

-- |A noun, which includes the NN, NNS, NNP, and NNPS Tags, and the underlying text.
data Noun = Noun Tag String
  deriving(Show, Eq)

-- |An Object is a noun with adjectives that describe it.
data Object = Object [Adjective] Noun
  deriving(Show, Eq)

-- |A Verb, which includes the VB, VBP, VBZ, VBN, VBG, and VBD Tags, and the underlying text.
data Verb = Verb Tag String
  deriving(Show, Eq)

-- |An NLTree is the root for many different kinds of sentences that we support. Currently,
--  we only support a verb-object pair.
data NLTree = VerbObject Verb Object
    deriving(Show, Eq)

-- |Parses the token stream into an NLTree, returning Nothing if no corresponding NLTree can be
-- constructed
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
