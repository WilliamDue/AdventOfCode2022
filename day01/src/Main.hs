module Main where

import Data.List.Split as S
import Data.List as L

parse :: String -> [[Int]]
parse = map (map read . words) . S.splitOn "\n\n"

solve1 :: [[Int]] -> Int
solve1 = L.maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . L.reverse . sort . map sum

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
