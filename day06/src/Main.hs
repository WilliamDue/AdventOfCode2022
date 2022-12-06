module Main where

import Data.List ( mapAccumL, nub )

substrings :: Int -> [Char] -> [[Char]]
substrings n = snd . mapAccumL (\a b -> (b:a, take n a)) ""

solve :: Int -> [Char] -> Int
solve n = length . takeWhile ((/=n) . length . nub) . substrings n

main :: IO ()
main = do
    contents <- getContents
    print $ solve 4 contents
    print $ solve 14 contents

