{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Set as S
import qualified Data.List as L
import Data.Foldable ( maximumBy, minimumBy ) 
import Data.Maybe ( fromJust, isJust )
import Data.Tuple ( swap )
import Debug.Trace ( traceShow , traceShowId )

data Direction = L | R deriving (Show, Eq)
type Point = (Int, Int)
type Shape = S.Set Point

instance Read Direction where
    readsPrec _ "<" = [(L, "")]
    readsPrec _ ">" = [(R, "")]
    readsPrec _ c = error ("Cannot parse \"" ++ c ++ "\".")

parse :: String -> [Direction]
parse = fmap (read . (:[]))

toXCoordinate :: String -> [Int]
toXCoordinate = fmap fst . L.filter ((=='#') . snd) . zip [0..]

toCoordinates :: String -> Shape
toCoordinates string = S.map swap . S.fromList . concat $ zipWith (\a b -> fmap (a,) b) rowIndices xCoordinates
    where lines' = lines string
          size = length lines'
          rowIndices = reverse [0..size - 1]
          xCoordinates = fmap toXCoordinate lines'

shapes :: [Shape]
shapes = (translate (-1, 0) . toCoordinates) <$> ["####", ".#.\n###\n.#.", "..#\n..#\n###", "#\n#\n#\n#", "##\n##"] 

translate :: Point -> Shape -> Shape
translate point = S.map (add point)
    where add (x, y) (x', y') = (x + x', y + y') 

moveDirection :: Direction -> Shape -> Shape
moveDirection L = translate (-1, 0)
moveDirection R = translate (1, 0)

moveDown :: Shape -> Shape
moveDown = translate (0, -1)

hitFloor :: Shape -> Bool
hitFloor = any (<0) . S.map snd

hitShape :: Shape -> Shape -> Bool
hitShape dynShape staShape = any (`S.member` staShape) dynShape

stopMoving :: Shape -> Shape -> Bool
stopMoving dynShape staShape = hitFloor dynShape || hitShape dynShape staShape

isInBound :: Shape -> Bool
isInBound = all (\x -> -3 <= x && x <= 3) . S.map fst

move :: [Direction] -> Shape -> Shape -> ([Direction], Shape)
move [] _ staShape = ([], staShape)
move (d:ds) dynShape staShape
    | stopMoving dynShape'' staShape = (ds, S.union dynShape' staShape)
    | otherwise = move ds dynShape'' staShape
    where temp = moveDirection d dynShape
          dynShape' = if isInBound temp && not (stopMoving temp staShape) then temp else dynShape
          dynShape'' = moveDown dynShape'

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c 

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c) 

maximumSnd ::Shape -> Point
maximumSnd = maximumBy (\a b -> (snd a) `compare` (snd b))

moveMany :: [Direction] -> [Shape] -> Shape -> Int -> Shape
moveMany ds (shape:shapes) staShape c
    | null ds' = staShape'
    | c <= 0 = staShape'
    | otherwise = moveMany ds' shapes staShape' (c - 1)
    where max' = if S.null staShape
                 then 0
                 else (1+) . snd $ maximumSnd staShape
          (ds', staShape') = move ds (translate (0, 3 + max') shape) staShape

solve1 :: Int -> [Direction] -> Int
solve1 n ds =  (1+)  .  snd . maximumSnd $ moveMany (cycle ds) (cycle shapes) S.empty (n - 1)

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 2022 input
    -- print $ solve 100000 input