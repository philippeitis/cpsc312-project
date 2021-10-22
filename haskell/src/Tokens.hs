module Tokens (
    Tag(..),
    Token(..),
    newTag,
    newToken,
) where

-- |A tag is a part of speech belonging to a specific word.
-- This is a subset of the tags that spaCy can produce.
data Tag = NN | NNS | NNP | NNPS
 | VB | VBP | VBZ |VBN | VBG | VBD
 | JJ | JJR | JJS
 | RB | PRP | DT | IN | CC | MD | TO
 | DOT | COMMA | EXCL | UH
  deriving(Show, Eq)

-- |A token is a tag and the underlying raw text.
data Token = Token Tag String
  deriving(Show, Eq)

-- |Parses a subset of known spaCy output tags
newTag :: String -> Maybe Tag
newTag "NN" = Just NN
newTag "NNS" = Just NNS
newTag "NNP" = Just NNP
newTag "NNPS" = Just NNPS
newTag "VB" = Just VB
newTag "VBP" = Just VBP
newTag "VBZ" = Just VBZ
newTag "VBN" = Just VBN
newTag "VBG" = Just VBG
newTag "VBD" = Just VBD
newTag "JJ" = Just JJ
newTag "JJR" = Just JJR
newTag "JJS" = Just JJS
newTag "RB" = Just RB
newTag "PRP" = Just PRP
newTag "DT" = Just DT
newTag "IN" = Just IN
newTag "UH" = Just UH
newTag _ = Nothing

-- |Produces a new token from a tag followed by text
newToken :: String -> String -> Maybe Token
newToken tag word = newTag tag >>= \tag -> Just (Token tag word)
