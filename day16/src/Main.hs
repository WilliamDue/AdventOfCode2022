{-# LANGUAGE TupleSections #-}
module Main where

import Debug.Trace
import qualified Data.Map as M
import Data.List.Split (splitOn)
import qualified Data.Char as C
import qualified Data.List as L
import Data.Bifunctor (first, second, bimap)
import Data.Maybe ( fromJust, isNothing ) 

data Node = Node { key :: String,
                   weight :: Int,
                   neighbours :: [String],
                   isOpen :: Bool } deriving (Show, Eq)

type Graph = M.Map String Node
data Tree a = TreeNode a [Tree a] | Leaf deriving (Show, Eq)

instance Functor Tree where
    fmap f (TreeNode a ls) = TreeNode (f a) (fmap (fmap f) ls)
    fmap f Leaf = Leaf

parseLine :: String -> Node
parseLine str = Node { key = key', weight = weight', neighbours = neighbours', isOpen = False }
    where [head', tail'] = splitOn ";" str
          key' = take 2 $ drop 6 head'
          weight' = read . takeWhile C.isDigit $ dropWhile (not . C.isDigit) head'
          neighbours' = splitOn "," $ filter (\c -> C.isUpper c || c == ',') tail'

parse :: String -> Graph
parse = M.fromList . map ((\n -> (key n, n)) . parseLine) . lines

bfs :: Graph -> String -> Int -> M.Map String Int
bfs graph source maxLength = helper graph queue pathLength maxLength
    where queue = [(source, 1)]
          pathLength = M.empty
          helper :: Graph -> [(String, Int)] -> M.Map String Int -> Int -> M.Map String Int
          helper graph' queue' pathLength' maxLength'
            | null queue' = pathLength'
            | weight + 1 > maxLength' = pathLength'
            | otherwise = helper graph' queue'' pathLength'' maxLength'
            where (pivot, weight) = head queue'
                  pathLength'' = M.insert pivot weight pathLength'
                  unexplored = filter (flip M.notMember pathLength') . neighbours $ graph' M.! pivot
                  queue'' = (tail queue') ++ map (,weight + 1) unexplored

maxNodes :: Graph -> String -> Int -> [(String, Int)]
maxNodes graph source maxLength = M.toList pathLengths'
    where pathLengths = bfs graph source maxLength
          isNotOpened = M.filter ((/=0) . weight) $ M.filter (not . isOpen) graph
          pathLengths' = M.filterWithKey (\k _ -> k `M.member` isNotOpened) pathLengths

updateIsOpen :: Graph -> String -> Graph 
updateIsOpen = flip $ M.adjust (\node -> node { isOpen = True })

mkTree :: Int -> Graph -> String -> Int -> Tree (String, Int)
mkTree maxLength graph source pathLength
    | maxLength < pathLength = Leaf
    | null nodes = TreeNode (source, (weight' * (maxLength - pathLength))) [Leaf]
    | otherwise = TreeNode (source, (weight' * (maxLength - pathLength))) trees
    where nodes = maxNodes graph source (maxLength - pathLength)
          (keys, pathLengths) = unzip nodes
          graphs = map (updateIsOpen graph) keys
          pathLengths' = map (+pathLength) pathLengths
          weight' = weight $ graph M.! source
          trees = zipWith3 (mkTree maxLength) graphs keys pathLengths'

treeSum :: Tree Int -> Int
treeSum (TreeNode a ls) = (+a) . maximum $ map treeSum ls
treeSum Leaf = 0

treeScan :: (a -> b -> b) -> b -> Tree a -> Tree b
treeScan _ _ Leaf = Leaf
treeScan f lst (TreeNode a ls) = TreeNode b (map (treeScan f (f a lst)) ls)
    where b = f a lst

root :: Tree a -> a
root (TreeNode a _) = a
root Leaf = error "Could not get root."

treeScan1 :: (a -> a -> a) -> Tree a -> Tree a
treeScan1 f tree = treeScan f a tree
    where a = root tree

solve1 :: Graph -> Int
solve1 graph = treeSum . fmap snd $ mkTree 30 graph "AA" 0

subTrees :: Tree a -> [Tree a]
subTrees (TreeNode _ ls) = ls
subTrees Leaf = []

paths :: Tree a -> [[a]]
paths tree = helper $ treeScan (:) [] tree
    where helper (TreeNode a ls) = a : concatMap helper ls
          helper Leaf = []

maxComb paths' = L.maximumBy (\(a, b) (c, d) -> (snd a + snd b) `compare` (snd c + snd d)) $ map (\p -> (p, max' $ filter (all (`notElem` (fst p)) . fst) paths')) paths'
    where max' = L.maximumBy (\a b -> snd a `compare` snd b)

collapse :: [(String, Int)] -> ([String], Int)
collapse = second sum . unzip

solve2 graph = uncurry (+)
               . bimap snd snd 
               . maxComb
               . map (collapse . init) 
               . paths 
               $ mkTree 26 graph "AA" 0

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input