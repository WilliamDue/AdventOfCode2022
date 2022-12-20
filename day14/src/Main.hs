module Main where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List.Split ( splitOn )
import Data.List ( nub, groupBy, sortOn, or, find )
import Data.Maybe ( isNothing, fromJust, isJust, catMaybes )

type MapState = Array (Int, Int) Char

interpolatePoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
interpolatePoints (x, y) (x', y')
    | x == x' = [(x, n) | n <- [(min y y') .. (max y y')]]
    | y == y' = [(n, y) | n <- [(min x x') .. (max x x')]]
    | otherwise = error "Could not interpolate."

interpolate :: [(Int, Int)] -> [(Int, Int)]
interpolate points = nub . concatMap (uncurry interpolatePoints) $ zip points (tail points)

parsePoint :: String -> (Int, Int)
parsePoint = (\[a, b] -> (read a, read b)) . splitOn ","

parseLine :: String -> [(Int, Int)]
parseLine = map parsePoint . splitOn " -> "

parse :: [Char] -> MapState
parse str = (initArray A.//) $ ((500, 0), '+') : zip walls (repeat '#')
    where walls = concatMap (interpolate . parseLine) $ lines str
          xVals = (500:) $ map fst walls
          yVals = (0:) $ map snd walls
          (minX, maxX) = (minimum xVals, maximum xVals)
          (minY, maxY) = (minimum yVals, maximum yVals)
          bounds' = ((minX, minY), (maxX, maxY))
          initArray = A.array bounds' $ zip (A.range bounds') (repeat '.')

printArray :: MapState -> IO ()
printArray state = putStrLn . unlines $ map (map (state A.!)) indices'
    where indices' = groupBy (\(_, y) (_, y') -> y == y') . sortOn snd . A.range $ A.bounds state

safeLookUp :: MapState -> (Int, Int) -> Maybe Char
safeLookUp state (x, y)
    | minX <= x && x <= maxX && minY <= y && y <= maxY = Just $ state A.! (x, y)
    | otherwise = Nothing
    where ((minX, minY), (maxX, maxY)) = A.bounds state

moveOnce :: MapState -> (Int, Int) -> Maybe (Int, Int)
moveOnce state (x, y)
    | isNothing cs = Nothing
    | otherwise = do
        cs' <- cs
        case fmap fst $ find ((=='.') . snd) cs' of
            Nothing -> Just (x, y)
            a -> a
    where moves = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
          cs = sequence $ map (\p -> do x <- safeLookUp state p; return (p, x)) moves

move :: (Int, Int) -> MapState -> Maybe MapState
move idx state = do
    newIdx <- moveOnce state idx
    if newIdx == idx
    then Just $ state A.// [(newIdx, 'o')]
    else move newIdx state

isEnd :: MapState -> Bool
isEnd = (=='+') . (A.! (500, 0))

iterateMove :: MapState -> [MapState]
iterateMove = catMaybes . takeWhile maybeIsEnd . iterate (>>= (move (500, 0))) . Just
    where maybeIsEnd state = case fmap isEnd state of
                                Nothing -> False
                                a -> fromJust a

solve :: MapState -> Int
solve = length . init . iterateMove

upgrade :: MapState -> MapState
upgrade state = newArray A.// (A.assocs state) A.// floor
    where ((minX, minY), (maxX, maxY)) = A.bounds state
          newBounds = ((minX - maxY - 2, minY), (maxX + maxY + 2, maxY + 2))
          newArray = A.array newBounds $ zip (A.range newBounds) (repeat '.')
          floor = [((x, snd $ snd newBounds), '#') | x <- [(fst $ fst newBounds)..(fst $ snd newBounds)]]

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve input
    print . (+1) . solve $ upgrade input
