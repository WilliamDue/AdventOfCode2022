module Main where

import Data.Text (toTitle, pack, unpack)
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Bifunctor ( Bifunctor(second) )

data Command = Addx Int | Noop deriving (Show, Read)

parse :: String -> [Command]
parse = map read . lines . unpack . toTitle . pack

execTime :: Command -> (Int, Command)
execTime Noop = (1, Noop)
execTime (Addx n) = (2, Addx n)

execAddTime :: (Int, Command) -> (Int, Command) -> (Int, Command)
execAddTime (n0, cmd0) (n1, cmd1) = (n0 + n1, addCommands cmd1 cmd0)

execute :: [Command] -> [(Int, Command)]
execute = scanl execAddTime (0, Addx 1) . map execTime

addCommands :: Command -> Command -> Command
addCommands (Addx a) (Addx b) = Addx $ a + b
addCommands (Addx a) _ = Addx a
addCommands _ (Addx a) = Addx a
addCommands _ _ = Noop

splitByPred :: (a -> b -> Bool) -> [a] -> [b] -> [[b]]
splitByPred _ _ [] = []
splitByPred _ [] _ = []
splitByPred f (x:xs) ls
    | null before = []
    | otherwise = before : splitByPred f xs (dropWhile (f x) ls)
    where before = takeWhile (f x) ls


findRegXAtFreq :: [Int] -> [(Int, Command)] -> [(Int, Int)]
findRegXAtFreq freqs = zip freqs 
                       . map (getCmdInt . snd)
                       . map last
                       . splitByPred (\a (b, _) -> b < a) freqs

getCmdInt :: Command -> Int
getCmdInt Noop = error "Can not get an Int from Noop."
getCmdInt (Addx n) = n 

solve1 :: [Command] -> Int
solve1 = sum . map (uncurry (*)) . findRegXAtFreq freqs . execute
    where freqs = [20, 60, 100, 140, 180, 220]

regXIntervals :: (Ord a, Num a, Enum a) => a -> [(a, b)] -> [[(a, b)]]
regXIntervals n = splitByPred (\a (b, _) -> b < a) (map (n*) [1..])

interpolateCycles :: [(Int, Command)] -> [(Int, Command)]
interpolateCycles [] = []
interpolateCycles [p] = [p]
interpolateCycles ((n, cmd):xs)
    | n == n' = interpolateCycles xs
    | otherwise = (n, cmd) : interpolateCycles ((n+1, cmd):xs)
    where (n', _) = head xs

padCycles :: (Int, Int) -> [(Int, Command)] -> [(Int, Command)]
padCycles (a, b) ls = leftPadding ++ ls ++ rightPadding 
    where (n, cmd) = head ls
          (n', cmd') = last ls
          leftPadding = [(x, cmd) | x <- [(a + 1)..n]]
          rightPadding = [(x, cmd') | x <- [n'..(b - 1)]]

preprocessCycles :: (Int, Int) -> [(Int, Command)] -> [(Int, Int)]
preprocessCycles interval = map (second getCmdInt) . padCycles interval . interpolateCycles

cyclesToLine :: [(Int, Int)] -> String
cyclesToLine [] = []
cyclesToLine ((n, x):ls) = let c = if x - 1 <= n && n <= x + 1 then '#'  else '.'
                           in c : cyclesToLine ls 

restClockCycle :: [Int] -> [[(Int, Int)]] -> [[(Int, Int)]]
restClockCycle ns = map (\(n, ls) -> map (subtractCycle n) ls) . zip ns
    where subtractCycle a (b, cmd) = (b - a, cmd) 

solve2 :: [Command] -> String
solve2 = unlines
         . map cyclesToLine
         . restClockCycle (map (40*) [0..])
         . init
         . chunksOf 40
         . preprocessCycles (0, 240)
         . execute

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    putStrLn $ solve2 input 
