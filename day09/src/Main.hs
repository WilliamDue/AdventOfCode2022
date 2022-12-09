module Main where

import Data.List ( mapAccumL, nub, iterate )
import Data.Bifunctor (Bifunctor(bimap))

data Direction = R Int | L Int | U Int | D Int deriving (Show, Read) 

parse :: String -> [Direction]
parse = map read . lines

distance :: (Int, Int) -> (Int, Int) -> (Int, Int)
distance (x, y) (x', y') = (x' - x, y' - y) 

moveHead :: (Int, Int) -> Direction -> [(Int, Int)]
moveHead (x, y) (R m) = [(x, y + n) | n <- [1..m]]  
moveHead (x, y) (L m) = [(x, y - n) | n <- [1..m]]
moveHead (x, y) (U m) = [(x + n, y) | n <- [1..m]]
moveHead (x, y) (D m) = [(x - n, y) | n <- [1..m]]

movesHead :: (Int, Int) -> [Direction] -> [(Int, Int)]
movesHead p = fst . mapAccumL (\a b -> (a ++ (moveHead (last a) b), a)) [p]

moveTail :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail n (x, y) (x', y') = if n < abs a || n < abs b then (x + signum a, y + signum b) else (x, y) 
    where (a, b) = distance (x, y) (x', y')

tailMove = scanl (moveTail 1) (0, 0) 

solve1 :: [Direction] -> Int
solve1 = length . nub . scanl (moveTail 1) (0, 0) . movesHead (0, 0)

solve2 :: [Direction] -> Int
solve2 = length . nub . (!! 9) . iterate tailMove . movesHead (0, 0)
main :: IO ()
main = do
    contents <- parse <$> getContents
    print $ solve1 contents
    print $ solve2 contents
