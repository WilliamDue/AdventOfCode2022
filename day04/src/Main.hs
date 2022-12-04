module Main where

import Data.List.Split ( splitOn )

type Range = (Int, Int)

parse :: String -> [(Range, Range)]
parse = map parseLine . lines
    where toTuple :: [a] -> (a, a)
          toTuple [a, b] = (a, b)
          toTuple _ = error "List must only contain two elements."

          parseRange :: String -> Range
          parseRange = toTuple . map read . splitOn "-"

          parseLine :: String -> (Range, Range)
          parseLine = toTuple . map parseRange . splitOn ","

isContained :: Range -> Range -> Bool
isContained (a, b) (c, d) = (c <= a && b <= d) || (a <= c && d <= b)  

isOverlap :: Range -> Range -> Bool
isOverlap (a, b) (c, d) = signum (b - c) /= signum (a - d)

solve1 :: [(Range, Range)] -> Int
solve1 = length . filter (uncurry isContained)

solve2 :: [(Range, Range)] -> Int
solve2 = length . filter (uncurry isOverlap)

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
