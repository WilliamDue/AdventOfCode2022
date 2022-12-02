module Main where

import qualified Data.Map as M

parse :: String -> [(Char, Char)]
parse = map toTuple . lines 
    where toTuple [a, _, b] = (a, b)
          toTuple _ = error "Invalid input"

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

scoreHand :: Shape -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3

(-<) :: Shape -> Shape -> Bool
(-<) Rock Paper = True
(-<) Scissors Rock = True
(-<) Paper Scissors = True
(-<) _ _ = False

score :: Shape -> Shape -> Int
score a b 
    | a == b = 3 + handScore
    | a -< b = 6 + handScore
    | otherwise = 0 + handScore
    where handScore = scoreHand b

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

strategy1 :: (Char, Char) -> (Shape, Shape)
strategy1 = (mapTuple ((M.!) strategy))
    where strategy = M.fromList [('A', Rock), ('X', Rock), ('B', Paper), ('Y', Paper),
                                 ('C', Scissors), ('Z', Scissors)]

solve1 :: [(Char, Char)] -> Int
solve1 = sum . map (uncurry score) . map strategy1

strategy2 :: (Char, Char) -> (Shape, Shape)
strategy2 ('A', 'X') = (Rock, Scissors)
strategy2 ('A', 'Y') = (Rock, Rock)
strategy2 ('A', 'Z') = (Rock, Paper)
strategy2 ('B', 'X') = (Paper, Rock)
strategy2 ('B', 'Y') = (Paper, Paper)
strategy2 ('B', 'Z') = (Paper, Scissors)
strategy2 ('C', 'X') = (Scissors, Paper)
strategy2 ('C', 'Y') = (Scissors, Scissors)
strategy2 ('C', 'Z') = (Scissors, Rock)

solve2 :: [(Char, Char)] -> Int
solve2 = sum . map (uncurry score) . map strategy2

main :: IO ()
main = do
  str <- getContents
  print . solve1 $ parse str
  print . solve2 $ parse str