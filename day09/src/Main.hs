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

directionToCoords :: [Direction] -> [(Int, Int)]
directionToCoords = fst . mapAccumL (\a b -> (a ++ (moveHead (last a) b), a)) [(0, 0)]

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (x', y') = if 1 < abs a || 1 < abs b then (x + signum a, y + signum b) else (x, y) 
    where (a, b) = distance (x, y) (x', y')

moves :: [(Int, Int)] -> [(Int, Int)]
moves = scanl move (0, 0) 

solve1 :: [Direction] -> Int
solve1 = length . nub . moves . directionToCoords

solve2 :: [Direction] -> Int
solve2 = length . nub . (!! 9) . iterate moves . directionToCoords

main :: IO ()
main = do
    contents <- parse <$> getContents
    print $ solve1 contents
    print $ solve2 contents
