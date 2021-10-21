module Tokens (
    Tag(..),
    Token(..),
    newTag,
    newToken,
) where

data Tag = NN | NNS | NNP | NNPS
 | VB | VBP | VBZ |VBN | VBG | VBD
 | JJ | JJR | JJS
 | RB | PRP | DT | IN | CC | MD | TO
 | DOT | COMMA | EXCL | UH
 deriving (Show)

data Token = Token Tag String deriving (Show)

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

newToken :: String -> String -> Maybe Token
newToken tag word = newTag tag >>= \tag -> Just (Token tag word)
