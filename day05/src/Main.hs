module Main where

import Data.List.Split ( splitOn )
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.IntMap as M

data Instruction = Instruction {move :: Int, from :: Int, to :: Int} deriving (Show, Eq)
type State = M.IntMap String

parse :: [Char] -> (State, [Instruction])
parse str = (parseState initState, parseProcedure procedure)
    where [initState, procedure] = splitOn "\n\n" str

          parseProcedure :: String -> [Instruction]
          parseProcedure = map parseInstruction . lines

          parseInstruction = aux . words
              where aux ["move", a, "from", b, "to", c] = Instruction (read a) (read b) (read c) 
                    aux str = error $ "Could not parse line " ++ concat str

          removeSpaces = filter (not . C.isSpace)
          cleanInitState = filter (any C.isAlpha) . map removeSpaces . L.transpose . lines

          parseStack :: String -> (Int, String)
          parseStack n = (read $ dropWhile (not . C.isNumber) n, init n)

          parseState :: String -> State
          parseState = M.fromList . map parseStack . cleanInitState

execute :: Bool -> State -> Instruction -> State
execute withRev state instruction = state''
    where move' = move instruction
          from' = from instruction
          to' = to instruction
          toMove = take move' . (state M.!) $ from' -- Get elements from the "from" stack
          state' = M.adjust (drop move') from' state -- Drop elemsn from the "from" stack
          toMove' = if withRev then L.reverse toMove else toMove
          state'' = M.adjust (toMove' ++) to' state' -- Add to the "to" stack

executeAll :: Bool -> State -> [Instruction] -> State
executeAll withRev state = foldl (execute withRev) state

solve :: Bool -> (State, [Instruction]) -> [Char]
solve withRev = M.foldr (\a b -> head a : b ) "" . uncurry (executeAll withRev)

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve True input
    print $ solve False input
