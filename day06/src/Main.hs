module Main where

import Data.List ( mapAccumL, nub )

solve :: Int -> [Char] -> Int
solve n = length . takeWhile ((/=n) . length . nub) . snd . mapAccumL (\a b -> (b:a, take n a)) ""

main :: IO ()
main = do
    contents <- getContents
    print $ solve 4 contents
    print $ solve 14 contents
