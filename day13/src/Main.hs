module Main where

import qualified Data.List as L
import Data.List.Split ( splitOn )
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R
import Control.Applicative ((<|>))
import qualified Data.Char as C
import Data.Maybe ( catMaybes, fromJust ) 
import Text.Read ( readMaybe ) 

data NestedList a = List [NestedList a] | Elem a

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

main :: IO ()
main = do
    input <- parse <$> getContents
    mapM_ print input

