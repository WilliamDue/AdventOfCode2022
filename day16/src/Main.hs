{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import qualified Data.Char as C
import qualified Data.List as L
import Data.Bifunctor (first, second)

data Node = Node { key :: String,
                   weight :: Int,
                   neighbours :: [String],
                   isOpen :: Bool } deriving (Show, Eq)

type Graph = M.Map String Node
data Tree a = TreeNode a [Tree a] | Leaf deriving (Show, Eq)

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

helper :: Int -> Graph -> String -> Int -> Tree Int
helper maxLength graph source pathLength
    | maxLength < pathLength = Leaf
    | null nodes = TreeNode (weight' * (maxLength - pathLength)) [Leaf]
    | otherwise = TreeNode (weight' * (maxLength - pathLength)) $ zipWith3 (helper maxLength) graphs keys pathLengths'
    where nodes = maxNodes graph source (maxLength - pathLength)
          (keys, pathLengths) = unzip nodes
          graphs = map (updateIsOpen graph) keys
          pathLengths' = map (+pathLength) pathLengths
          weight' = weight $ graph M.! source

treeSum :: Tree Int -> Int
treeSum (TreeNode a ls) = (+a) . maximum $ map treeSum ls
treeSum Leaf = 0

solve1 :: Graph -> Int
solve1 graph = treeSum $ helper 30 graph "AA" 0


main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    -- mapM_ putStrLn $ M.keys input
    -- mapM_ putStr $ map (\(k, n) -> unlines . map ((k ++ " ") ++) $ neighbours n) $ M.toList input
    -- print $ input