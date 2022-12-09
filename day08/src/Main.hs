module Main where

import Data.Array ( Ix(range), array, Array, (!), bounds, (//), indices)
import Data.List ( transpose, groupBy, group, sortOn, nub, mapAccumL )
import Data.Char ( digitToInt ) 
import Data.Maybe ( catMaybes, isNothing, fromJust, listToMaybe )
import Data.Tuple ( swap )
import Data.Bifunctor ( Bifunctor(first), Bifunctor(second) )

parse :: String -> Array (Int, Int) Int
parse str = array bounds' . zip indices' . concat $ ls
    where ls :: [[Int]]
          ls = map (map digitToInt) $ lines str
          y = length ls
          x = length $ head ls
          indices' = range ((0, 0), (y - 1, x - 1))
          bounds' = ((0, 0), (y - 1, x - 1))

idxExist :: (Ord a1, Ord a2) => Array (a1, a2) e -> (a1, a2) -> Bool
idxExist arr (x, y) = x0 <= x && x <= x1 && y0 <= y && y <= y1
    where ((x0, y0), (x1, y1)) = bounds arr

safeLookUp :: (Ix a1, Ix a2) => Array (a1, a2) a -> (a1, a2) -> Maybe a
safeLookUp arr idx = if idxExist arr idx
                     then Just (arr ! idx)
                     else Nothing

rows :: Array (Int, Int) Int -> [[(Int, Int)]]
rows = groupBy (\(x, _) (x', _) -> x == x') . sortOn fst . indices

colunms :: Array (Int, Int) Int -> [[(Int, Int)]]
colunms = groupBy (\(_, y) (_, y') -> y == y') . sortOn snd . indices

compare' :: (Ord a, Ix a1, Ix a2) => Array (a1, a2) a -> Maybe (a1, a2) -> Maybe (a1, a2) -> Bool
compare' arr (Just p) (Just p') = if (idxExist arr p) && (idxExist arr p')
                                  then (arr ! p) > (arr ! p')
                                  else False
compare' _ _ _ = True 

findVisibility arr = map fst
                        . filter (id . snd)
                        . uncurry zip
                        . first (reverse . map fromJust . init)
                        . mapAccumL (\a b -> (b:a, and $ map (compare' arr b) a)) [Nothing]
                        . map Just

solve1 arr = nub 
             . concat
             . concat
             $ map (map (findVisibility arr)) [left, right, top, bottom]
    where left = rows arr
          right = map reverse left
          top = colunms arr
          bottom = map reverse top

main :: IO ()
main = do
    input <- parse <$> getContents
    print . length $ solve1 input