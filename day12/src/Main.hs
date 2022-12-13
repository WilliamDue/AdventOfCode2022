module Main where

import qualified Data.Map as M
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import Data.Maybe (isJust, isNothing, catMaybes, fromJust)
import Data.Char (ord, isUpper)
import Data.Bifunctor ( Bifunctor(first) )
import qualified Data.List as L

data Distance = Infinity | Length Int deriving (Show, Eq)

instance Ord Distance where
    Length a `compare` Length b = a `compare` b
    Infinity `compare` Infinity = EQ
    Infinity `compare` _ = GT
    _ `compare` Infinity = LT 

data Item = Item { distance :: Distance, key :: (Int, Int) } deriving (Show, Eq)

instance Ord Item where
    Item a _ `compare` Item b _ = a `compare` b

instance Num Distance where
    Infinity + a = Infinity
    a + Infinity = Infinity
    Length a + Length b = Length (a + b)
    Infinity * a = Infinity
    a * Infinity = Infinity
    Length a * Length b = Length (a * b)
    abs Infinity = Infinity
    abs (Length a) = Length $ abs a
    signum Infinity = Infinity
    signum (Length a) = Length $ signum a
    fromInteger a = Length $ fromInteger a
    negate Infinity = Infinity
    negate (Length a) = Length $ negate a


parse :: String -> M.Map (Int, Int) Char 
parse str = (M.fromList . concat . map helper . zip [1..y]) matrix
    where matrix = lines str
          y = length matrix
          helper (y', ls) = zip [(x', y') | x' <- [1..length ls]] ls

isKey :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
isKey map' pivotKey neighbourKey = isJust result
    where result = do
                   pivot <- M.lookup pivotKey map'
                   neighbour <- M.lookup neighbourKey map'
                   let canMoveTo = ord neighbour <= ord pivot + 1 && neighbour /= 'E'
                   let isEnd = ((pivot == 'z') && (neighbour == 'E'))
                   let isStart = pivot == 'S'
                   if isStart || isEnd || canMoveTo then Just True else Nothing
                             
findNeighbour :: M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
findNeighbour map' (x, y) = filter (isKey map' (x, y)) neighbours
    where neighbours = [(x, y + 1),
                        (x, y - 1),
                        (x + 1, y),
                        (x - 1, y)]

mkGraph :: M.Map (Int, Int) Char -> M.Map (Int, Int) [(Int, Int)]
mkGraph map' = M.mapWithKey (\k _ -> findNeighbour map' k) map'

mkMinHeap :: M.Map (Int, Int) [(Int, Int)] -> (Int, Int) -> MinHeap Item
mkMinHeap graph source = H.insert (Item {distance = Length 0, key = source}) heap
    where heap = H.fromList 
                 . map (\k -> Item {distance = Infinity, key = k})
                 . filter (==source) $ M.keys graph

djiktra :: M.Map (Int, Int) [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Distance
djiktra graph' source' dest' = helper graph' heap' distances' dest'
    where distances' = M.insert source' (Length 0) . M.fromList $ zip (M.keys graph') (repeat Infinity)
          heap' = mkMinHeap graph' source'
          helper :: M.Map (Int, Int) [(Int, Int)] -> MinHeap Item -> M.Map (Int, Int) Distance -> (Int, Int) -> Distance
          helper graph heap distances dest
            | H.null heap = Infinity
            | otherwise = if (key node) == dest then newDistances M.! dest else helper graph newHeap newDistances dest
            where node = head $ H.take 1 heap
                  distance' = distance node
                  findNewDist val key val' = if Length 1 + val <= val'
                                             then Just (key, Length 1 + val)
                                             else Nothing 
                  newDistances = M.unionWith min distances . M.fromList $ fromJust altDistances
                  remove = map key heapChanges
                  newHeap' = H.filter (\x -> (key x) `notElem` remove) $ H.drop 1 heap
                  heapChanges =  map (\(key, dist) -> Item {distance = dist, key = key})
                                 $ fromJust altDistances
                  newHeap = H.union newHeap'
                            . H.fromList
                            . map (\(key, dist) -> Item {distance = dist, key = key})
                            $ fromJust altDistances
                  altDistances = do
                           neighbourKeys <- M.lookup (key node) graph
                           neighbours <- sequence $ map (flip M.lookup distances) neighbourKeys
                           let pairs = zip neighbourKeys neighbours
                           let changes = catMaybes $ map (uncurry (findNewDist distance')) pairs 
                           return changes


solve char map' = minimum $ map (flip (djiktra graph) dest) sources
    where graph = mkGraph map'
          mapLs = M.toList map'
          sources =  map fst $ filter ((==char) . snd) mapLs
          (dest, _) = fromJust $ L.find ((=='E') . snd) mapLs

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve 'S' input
    print $ solve 'a' input
