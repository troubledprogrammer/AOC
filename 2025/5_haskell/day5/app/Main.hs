module Main (main) where

import Text.Printf
import Data.List
import Data.Ord

loadInput :: Bool -> IO String
loadInput test = readFile ("../../../data/2025/5" ++ if test then "_test" else "")

parseRange :: String -> (Int, Int)
parseRange s =
    let (l, _: r) = break (== '-') s
    in (read l, read r)

parseInput :: String -> ([(Int, Int)], [Int])
parseInput input =
    let (ranges, _: values) = break null (lines input)
    in (map parseRange ranges, map read values)

inRange :: Int -> (Int, Int) -> Bool
inRange n (l, r) = n >= l && n <= r

-- union of intervals
unionSize :: [(Int, Int)] -> Int
unionSize ranges = 
    let merged = merge (sortBy (comparing fst) ranges)
    in sum (map rangeSize merged)
    where 
        rangeSize (l, r) = r - l + 1

        merge [] = []
        merge [x] = [x]
        merge ((l1, r1) : (l2, r2) : rest)
            | r1 >= l2 - 1 = merge ((l1, max r1 r2) : rest)
            | otherwise    = (l1, r1) : merge ((l2, r2) :  rest)

run :: ([(Int, Int)], [Int]) -> (Int, Int)
run (ranges, values) =
    let isInRange = map (\v -> any (inRange v)  ranges) values
        a = (length . filter id) isInRange
        b = unionSize ranges
    in (a, b)

main :: IO ()
main = do
    input <- loadInput False
    let parsed = parseInput input
        (a, b) = run parsed
    printf "Part a %d\nPart b %d\n" a b
