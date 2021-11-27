-- stack --resolver lts-18.6 script

import Control.Monad (forM)
import Data.Char (toLower)
import Data.Foldable (minimumBy)
import Data.Functor((<&>))
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap, empty, insert)
import qualified Data.HashMap.Strict(lookup)

data Connection = Delete | Subst | Insert deriving (Enum, Show)
type LevTable = [[(Float, Connection)]]

readNBytes :: Integer -> IO String
readNBytes n = forM [1..n] (const getChar)

substCost :: Char -> Char -> Float
substCost a b
    | a == b = 0.0
    | toLower a == toLower b = 0.3
    | otherwise = 1.0

minCost :: [(Float, Connection)] -> (Float, Connection)
minCost = minimumBy (compare `on` fst)

fillRow :: [(Float, Connection)] -> [(Float, Connection)] -> Char -> String -> [(Float, Connection)]
fillRow _ row cA "" = row
fillRow prevRow row  cA (cB:strB) =
    fillRow ((delete, deleteConn):prevRest) (minC:row) cA strB
    where
        ((subst, _):(delete, deleteConn):prevRest) = prevRow
        ((insert, _):rest) = row
        minC = minCost [(delete + 1.0, Delete),
            (subst + substCost cA cB, Subst),
            (insert + 1.0, Insert)]

runLevenshtein :: String -> String -> LevTable
runLevenshtein strA strB =
    foldl fillRowHelper [replicate (length strB + 1) (0.0, Subst)] (zip [1..] strA)
    where
        fillRowHelper :: LevTable -> (Int, Char) -> LevTable
        fillRowHelper (head:tail) (ind, cA) = (reverse $ fillRow head [(fromIntegral ind, Subst)] cA strB):head:tail
        fillRowHelper _ _ = undefined

backtrack :: LevTable -> ([(Int, Int)], Float)
backtrack ((_:row):_) = ([], fst $ minCost row)
backtrack _ = error "empty LevTable"

levenshtein :: String -> String -> Float
levenshtein sentAA sentBB = do
    let (sentA, sentB) = if length sentAA < length sentBB
        then (sentAA, sentBB)
        else (sentBB, sentAA)
    1.0 - snd (backtrack $ runLevenshtein sentA sentB) / fromIntegral (length sentA)

levenshteinCached :: HashMap (String, String) Float -> String -> String -> (Float, HashMap (String, String) Float)
levenshteinCached cache a b = case Data.HashMap.Strict.lookup (a, b) cache of
    Just f -> (f, cache)
    Nothing -> let distance = levenshtein a b
        in (distance, insert (a, b) distance cache)

levenshteinUser :: HashMap (String, String) Float -> [String] -> (Float, HashMap (String, String) Float)
levenshteinUser cache [a, b] = levenshteinCached cache a b
levenshteinUser _ _ = error "expected two items"

repl :: HashMap (String, String) Float -> IO ()
repl cache = getLine
    >>= mapM readNBytes . map read . splitOn " "
    <&> levenshteinUser cache
    >>= \(dist, newcache) -> putStrLn "OK"
        >> print dist
        >> repl newcache

main :: IO ()
main = repl empty