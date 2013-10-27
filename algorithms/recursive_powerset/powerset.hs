-- Haskell
-- real    0m0.293s

module Main where

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (elem:remaining) =
  let subpowersets = powerset remaining
  in  map (elem:) subpowersets ++ subpowersets

main :: IO ()
main = do
  putStrLn . show . length $ powerset [1..max]
  where max = 21