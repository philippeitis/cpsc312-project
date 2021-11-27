import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Rendering individuals works" $ do
      1 @?= 2
  -- NOTE: Uncomment this to see what a failing assertion looks like:
  -- , testCase "Bad assertion" $ do
  --     1 @?= 2
  -- NOTE: This is how to explicitly assert failures:
  -- , testCase "Explicit failure" $ do
  --     assertFailure "BOOM!"
  ]
