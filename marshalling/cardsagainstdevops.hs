
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
  gen <- newStdGen
  cards <- fmap (unzip . toList) . decode True <$> simpleHttp cardURL
  putStrLn $ either id (readCards . chooseCards gen) cards

readCards :: (String, String) -> String
readCards (white, black) = subRegex (mkRegex "_+") black white

chooseCards :: StdGen -> ([a], [a]) -> (a, a)
chooseCards gen = flip evalState gen . both randomElem

randomElem :: [a] -> State StdGen a
randomElem xs = state $ first (xs !!) . randomR (0, length xs - 1)




