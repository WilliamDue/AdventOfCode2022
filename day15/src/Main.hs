module Main where
    
import qualified Text.ParserCombinators.ReadP as R
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.List (nub, find, sort)
import Data.Maybe ( catMaybes, mapMaybe, listToMaybe ) 
import Control.Monad ((>>=), liftM2)

type Point = (Int, Int)
data Sensor = Sensor { location :: Point,
                       beacon :: Point,
                       distance :: Int} deriving (Show)

isNum :: Char -> Bool
isNum c = isDigit c || c == '-' 

skipAlpha :: R.ReadP ()
skipAlpha = R.skipMany1 (R.munch1 (not . isNum)) 

parseSensor :: String -> Sensor
parseSensor = toSensor . fst . last . R.readP_to_S (R.many int)
    where int = do skipAlpha; read <$> R.munch1 isNum
          toSensor [a, b, c, d] = Sensor (a, b) (c, d) (dist (a, b) (c, d))

parse :: String -> [Sensor]
parse = fmap parseSensor . lines

(|-|) :: Point -> Point -> Point
(|-|) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

abs2 :: Point -> Point
abs2 = bimap abs abs

dist :: Point -> Point -> Int
dist = ((uncurry (+) . abs2) .) . (|-|)

bounds :: [Sensor] -> (Point, Point)
bounds sensors = ((minX, minY), (maxX, maxY))
    where dists = distance <$> sensors
          locations = location <$> sensors
          xVals = fst <$> locations 
          yVals = snd <$> locations
          minX = minimum $ zipWith (-) xVals dists
          maxX = maximum $ zipWith (+) xVals dists
          minY = minimum $ zipWith (-) yVals dists
          maxY = maximum $ zipWith (+) yVals dists

splitInterval :: Int -> Sensor -> Point -> Maybe Point
splitInterval y sensor (a, b)
    | xDist < 0 = Nothing
    | otherwise = Just (x' - xDist, x' + xDist)
    where dist' = distance sensor 
          (x', y') = location sensor
          yDist = abs $ y - y'
          xDist = dist' - yDist

splitIntervals :: Int -> [Sensor] -> Point -> [Point]
splitIntervals y sensors = concatIntervals . catMaybes . zipWith (splitInterval y) sensors . repeat

isOverlapping :: Point -> Point -> Bool
isOverlapping (x1, x2) (y1, y2) = max x1 y1 <= min x2 y2

concatInterval :: Point -> Point -> Maybe Point
concatInterval (x1, x2) (y1, y2) 
    | isOverlapping (x1, x2) (y1, y2) = Just (min x1 y1, max x2 y2)
    | otherwise = Nothing

concatIntervals :: [Point] -> [Point]
concatIntervals [] = []
concatIntervals (i:is)
    | any (isOverlapping i) is = concatIntervals newIntervals 
    | otherwise = i : concatIntervals is
    where newIntervals = (filter (not . isOverlapping i) is) ++ mapMaybe (concatInterval i) is

solve1 :: Int -> [Sensor] -> Int
solve1 y sensors = sum $ map (abs . uncurry (-)) intervals
    where ((minX, minY), (maxX, maxY)) = bounds sensors
          intervals = splitIntervals y sensors (minX, maxX)

-- https://gist.github.com/Dimanaux/fc635a2033a0a0a9ea49ab2fb8622702
-- Took this function.
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

subsets2 :: [a] -> [[a]]
subsets2 = filter ((==2) . length) . subsets

isBeacon :: Point -> Point -> Maybe Int
isBeacon (x1, x2) (y1, y2)
    | abs (x2 - y1) == 2 = Just $ (min x2 y1) + 1
    | abs (x1 - y2) == 2 = Just $ (min x1 y2) + 1
    | otherwise = Nothing

maybeBeaconX y sensors (minX, maxX) = listToMaybe
                                      . mapMaybe (\y -> do x <- toTuple y; uncurry isBeacon x) 
                                      . subsets2 $ splitIntervals y sensors (minX, maxX) 
    where toTuple [a, b] = Just (a, b)
          toTuple _ = Nothing

solve2 :: Int -> [Sensor] -> Int
solve2 upper sensors = (\(x, y) -> x * 4000000 + y) $ head intervals
    where ((minX, minY), (maxX, maxY)) = ((0, 0), (upper, upper))
          intervals = mapMaybe (\y -> do x <- maybeBeaconX y sensors (minX, maxX); return (x, y)) [minY..maxY]

main :: IO ()
main = do
    input <- parse <$> getContents
    -- print $ solve 10 input -- test
    print $ solve1 2000000 input
    -- print $ solve2 20 input
    print $ solve2 4000000 input