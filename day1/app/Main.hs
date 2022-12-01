module Main where

import Data.List.Split as S
import Data.List as L
import qualified MyLib (someFunc)

parse :: String -> [[Int]]
parse = map (map read . words) . S.splitOn "\n\n"

solve1 :: [[Int]] -> Int
solve1 = L.maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . L.reverse . sort . map sum

main :: IO ()
main = do
  str <- getContents
  print . solve1 $ parse str
  print . solve2 $ parse str
