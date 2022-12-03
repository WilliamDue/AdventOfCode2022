module Main where

import qualified Data.Set as S
import qualified Data.Char as C
import Data.List.Split (chunksOf)

parse :: String -> [String]
parse = lines

splitAtMid :: [a] -> [[a]]
splitAtMid ls = (\(a, b) -> [a, b]) $ splitAt mid ls
    where mid = length ls `div` 2

findDuplicateItem :: [String] -> Char
findDuplicateItem = S.findMin . foldl1 S.intersection . map S.fromList

priority :: Char -> Int
priority c
    | C.isLower c = C.ord c - 96
    | C.isUpper c = C.ord c - 38
    | otherwise = error "Only alphabetic chars are allowed"

solve1 :: [String] -> Int
solve1 = sum . map (priority . findDuplicateItem . splitAtMid)

solve2 = sum . map (priority . findDuplicateItem) . chunksOf 3

main :: IO ()
main = do
        contents <- getContents
        print . solve1 $ parse contents
        print . solve2 $ parse contents
