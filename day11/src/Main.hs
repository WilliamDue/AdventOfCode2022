module Main where

import Data.List.Split ( splitOn )
import qualified Data.List as L
import Data.Char ( isSpace, isNumber ) 
import qualified Data.Map as M
import Data.Bifunctor ( first ) 

data Monkey = Monkey { key :: Int, 
                       items :: [Integer], 
                       operation :: Integer -> Integer,
                       test :: Integer -> Int,
                       count :: Int,
                       divisor :: Integer }

instance Show Monkey where
    show (Monkey k s _ _ _ _) = "Monkey " ++ show k ++ ": " ++ show s

parseKey :: String -> Int
parseKey = read . takeWhile isNumber . dropWhile (not . isNumber)

parseitems :: String -> [Integer]
parseitems = fmap read . splitOn ", " . dropWhile (not . isNumber)

lcm' :: [Integer] -> Integer
lcm' = foldl1 Prelude.lcm

parseOperation :: String -> (Integer -> Integer)
parseOperation = helper . words . L.drop 1 . dropWhile (/='=')
    where helper ["old", "+", "old"] = \old -> old + old 
          helper ["old", "+", a] = \old -> old + (read a)
          helper [a, "+", "old"] = helper ["old", "+", a]
          helper ["old", "*", "old"] = \old -> old * old 
          helper ["old", "*", a] = \old -> old * (read a)
          helper [a, "*", "old"] = helper ["old", "*", a]
          helper _ = error "Invalid operation for a Monkey"

parseInt :: String -> Int
parseInt = read . dropWhile (not . isNumber)

parseTest :: String -> String -> String -> (Integer -> Int)
parseTest test' true' false' = \x -> if x `mod` test'' == 0 then true'' else false''
    where test'' = toInteger $ parseInt test'
          true'' = parseInt true'
          false'' = parseInt false'

parseMonkey :: String -> Monkey
parseMonkey = parseMonkey' . fmap (dropWhile isSpace) . lines

parseMonkey' :: [String] -> Monkey 
parseMonkey' [key', items', operation', test', true', false'] = 
    Monkey { key = parseKey key', 
             items = parseitems items',
             operation = parseOperation operation', 
             test = parseTest test' true' false',
             count = 0,
             divisor = toInteger $ parseInt test' }

parse :: String -> (Integer, M.Map Int Monkey)
parse s = (lcm' . map (divisor . snd) $ M.toList monkeys, monkeys)
    where monkeys = M.fromList . fmap (\m -> (key m, m) ) . fmap parseMonkey $ splitOn "\n\n" s 

computeWorry :: Integer -> Integer -> Monkey -> Integer -> (Int, Integer)
computeWorry a b monkey' n = ((test monkey') worry, worry)
    where worry = modulus . division $ (operation monkey') n
          modulus = if b == 0 then id else (`mod` b)
          division = if a == 0 then id else (`div` a)

updateMonkey :: [Integer] -> Monkey -> Monkey
updateMonkey n monkey = monkey { items = n ++ items monkey }

updateMonkeys :: M.Map Int Monkey -> [(Int, Integer)] -> M.Map Int Monkey
updateMonkeys monkeys updates = foldl helper monkeys itemsToAdd
    where itemsToAdd = map (\a -> (fst $ head a, map snd a) ) . L.group . L.sortOn fst $ updates
          helper :: M.Map Int Monkey -> (Int, [Integer]) -> M.Map Int Monkey
          helper m (key', items') = M.adjust (updateMonkey items') key' m  

inspect :: Integer -> Integer -> Int -> M.Map Int Monkey -> M.Map Int Monkey
inspect a b key' monkeys = updateMonkeys monkeys' updates
    where monkey' = flip (M.!) key' $ monkeys 
          items' = items monkey'
          monkey'' = monkey' { items = [], count = length items' + count monkey' }
          monkeys' = M.insert key' monkey'' monkeys
          updates = fmap (computeWorry a b monkey') items'

inspectAll :: Integer -> Integer ->  M.Map Int Monkey -> M.Map Int Monkey
inspectAll a b monkeys = helper keys monkeys
    where keys = M.keys monkeys
          helper [] monkeys' = monkeys' 
          helper (key':keys') monkeys' = helper keys' (inspect a b key' monkeys') 

solve :: Int -> Integer -> Integer -> M.Map Int Monkey -> Int
solve n a b = product 
              . take 2
              . L.sortBy (flip compare)
              . fmap (count . snd)
              . M.toList
              . last
              . take (n + 1)
              . iterate (inspectAll a b)

main :: IO ()
main = do
    (lcm, input) <- parse <$> getContents
    print $ solve 20 3 0 input
    print $ solve 10000 0 lcm input
