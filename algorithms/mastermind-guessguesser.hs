{-
  quick and dirty solution to sort of the thing mentioned in
  http://stackoverflow.com/questions/9202302/mastermind-game-in-c
  driven by brute force, and probably not very good.
  written mostly to vent after having failed miserably
  the last few hours with a non-brute force backtracking
  solution...

  the guesses variable contains a test set of constraints
  with two possible valid solutions.

      *Main> valids guesses
      [[1,2,3,4,5],[5,2,9,4,5]]
      *Main> existsSolution guesses
      True

  when there exists trivial solutions the existsSolutions function
  is pretty quick since it just needs the first solution to know
  that there are solutions at all.

  I have in mind a more effecient implementation where you just
  existentially prove that there indeed *are* solutions, simply by
  testing the guesses for contradictions. This will not give you any
  solutions, but if there are no contradictions in the guesses,
  there exists at least one solution.

-}

module Main where

import Control.Monad


-- a list of pairs of "guess" and "number of correct numbers"
guesses = [([5, 2, 3, 1, 7], 2),
           ([1, 3, 9, 5, 2], 1),
           ([7, 3, 8, 9, 3], 0),
           ([7, 2, 8, 4, 5], 3)]


-- works out how many correct numbers there are for a specific guess
corrects real guess =
  fromIntegral . length . filter id $ zipWith (\(a,b) if a == b then 1 else 0) real guess

-- checks if a candidate guess has the right number of numbers correct
valid candidate guess correctness = corrects candidate guess == correctness

-- filters out all the invalid candidates from a list of all the candidates there could possibly be
-- this is the brute force bit...
valids guesses = do
  candidate <- replicateM 5 [1..9]
  guard $ all (uncurry (valid candidate)) guesses
  return candidate

-- lazily check if there is at least one solution
existsSolution = not . null . take 1 . valids