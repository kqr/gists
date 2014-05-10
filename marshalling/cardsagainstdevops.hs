
-- A short program to generate sentences from the
-- first batch of "Cards Against DevOps".
-- Compile and run for comedy.


module Main where

import Network.HTTP.Conduit    (simpleHttp)
import Data.Csv                (decode)
import Data.Vector             (toList)

import System.Random           (StdGen,  newStdGen, randomR)
import Control.Monad.State     (State,   state,     evalState)

import Text.Regex              (mkRegex, subRegex)

import Control.Arrow           (first)
import Control.Lens            (both)
import Data.Functor            ((<$>))


cardURL = concat ["https://raw.githubusercontent.com"
                 ,"/bridgetkromhout/devops-against-humanity"
                 ,"/master/first-printing-cards-DevOpsAgainstHumanity.csv"]


main = do
  -- Get a pre-seeded random generator
  gen <- newStdGen

  -- Download and parse the CSV for the cards
  cards <- fmap (unzip . toList) . decode True <$> simpleHttp cardURL

  -- Either display an error message or read a randomly chosen pair of cards
  putStrLn $ either id (readCards . chooseCards gen) cards

-- Read cards by simply replacing any sequence of underscores ('_') in the
-- black card with the contents of the white card
readCards :: (String, String) -> String
readCards (white, black) = subRegex (mkRegex "_+") black white

-- Pick one card each from the two piles of black and white cards
-- Uses the State monad briefly to chain the two random selections
chooseCards :: StdGen -> ([String], [String]) -> (String, String)
chooseCards gen = flip evalState gen . both (state . randomElem)

-- Pick a random element from a list, given a random generator
-- Return the random element and the updated generator
randomElem :: [a] -> StdGen -> (a, StdGen)
randomElem xs = first (xs !!) . randomR (0, length xs - 1)




