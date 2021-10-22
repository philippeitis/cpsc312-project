import Test.Tasty
import Test.Tasty.HUnit

import Graphics.Gloss
import NLTree
import Parse
import Tokens (Tag (..), Token (..))
import World

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Rendering individuals works" $ do
      individualToPicture (Individual [Size Default, AColor black] Cat Center) @?=
        (Scale 1.0 1.0 (Color black (pictures [
              circle 40,
              line [(32, 24), (32, 36), (24, 32)],
              line [(-32, 24),  (-32, 36), (-24, 32)]
          ])))

  , testCase "Parsing color adjective works" $ do
     parseAdjective (Adjective JJ "black") @=? Just (AColor black)
  , testCase "Parsing other adjectives fails" $ do
     parseAdjective (Adjective JJ "angry") @=? Nothing
  , testCase "Parsing pretokenized sentence produces default cat" $ do
     parse [Token VB "make", Token DT "a", Token NN "cat"] @=? Just (VerbObject
                                                                       (Verb VB "make")
                                                                       (Object [] (Noun NN "cat"))
                                                                   )
  , testCase "Parsing pretokenized sentence with color produces tree with adjectives" $ do
     parse [Token VB "make", Token DT "a", Token JJ "black", Token NN "cat"] @=? Just (VerbObject
                                                                       (Verb VB "make")
                                                                       (Object [Adjective JJ "black"] (Noun NN "cat"))
                                                                   )

  -- NOTE: Uncomment this to see what a failing assertion looks like:
  -- , testCase "Bad assertion" $ do
  --     1 @?= 2
  -- NOTE: This is how to explicitly assert failures:
  -- , testCase "Explicit failure" $ do
  --     assertFailure "BOOM!"
  ]
