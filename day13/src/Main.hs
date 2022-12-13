module Main where

import qualified Data.List as L
import Data.List.Split ( splitOn )
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import Control.Applicative ((<|>))
import qualified Data.Char as C
import Data.Maybe ( catMaybes, fromJust ) 
import Text.Read ( readMaybe ) 
import Data.Bifunctor ( Bifunctor(second) )


data NestedList a = List [NestedList a] | Elem a deriving (Eq)

instance Show a => Show (NestedList a) where
    show (List ls) = "[" ++ (concat . L.intersperse "," $ map show ls) ++ "]"
    show (Elem n) = show n

parseInt :: ReadP (Maybe (NestedList Int))
parseInt = fmap (fmap Elem . readMaybe) $ R.munch C.isDigit

sep :: ReadP Char
sep = R.satisfy (==',')

open :: ReadP Char
open = R.satisfy (=='[')

close :: ReadP Char
close = R.satisfy (==']')

newline :: ReadP Char
newline = R.satisfy (=='\n')

parseList :: ReadP (Maybe (NestedList Int))
parseList = do 
    parsed <- R.sepBy (parseInt R.+++ parseLine) sep
    return . Just . List $ catMaybes parsed

parseLine :: ReadP (Maybe (NestedList Int))
parseLine = R.between open close parseList

parse :: [Char] -> [(NestedList Int, NestedList Int)]
parse = map (toTuple . map (fromJust . fst . head . R.readP_to_S parseLine) . lines) . splitOn "\n\n"
    where toTuple [a, b] = (a, b)
          toTuple _ = error "Could not parse."

compareLeft :: Ord a => NestedList a -> NestedList a -> Ordering
compareLeft (List (x:xs)) (List (y:ys)) = compareLeft x y
compareLeft (Elem a) (List (y:ys)) = compareLeft (Elem a) y
compareLeft (List (x:xs)) (Elem b) = compareLeft x (Elem b)
compareLeft (Elem a) (Elem b) = a `compare` b
compareLeft (List []) (List []) = EQ
compareLeft (List []) _ = LT
compareLeft _ (List []) = GT

removeLeft :: NestedList a -> NestedList a
removeLeft (List ((Elem _):xs)) = List xs
removeLeft (List ((List []):xs)) = List xs
removeLeft (List []) = List []
removeLeft (List (x:xs)) = List ((removeLeft x):xs)

compareNestedList :: Ord a => NestedList a -> NestedList a -> Ordering
compareNestedList a b
    | ordering == EQ = compareNestedList (removeLeft a) (removeLeft b)
    | otherwise = ordering
    where ordering = compareLeft a b

solve1 :: [(NestedList Int, NestedList Int)] -> Int
solve1 = sum . map fst . filter ((==LT) . (uncurry compareNestedList) . snd) . zip [1..]

solve2 :: [(NestedList Int, NestedList Int)] -> NestedList Int -> NestedList Int -> Int
solve2 pairPackages a b = aIdx * bIdx
    where packages = ([a, b]++) . (uncurry (++)) $ unzip pairPackages
          sortedPackages = L.sortBy compareNestedList packages
          aIdx = (1+) . fromJust $ L.findIndex (==a) sortedPackages
          bIdx = (1+) . fromJust $ L.findIndex (==b) sortedPackages

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input (List [ List [Elem 2]]) (List [ List [Elem 6]])
    