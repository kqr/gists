
-- Made for someone else to show a possible way of handling things in Haskell.
-- Generates a random pokemon battle in the style of http://viclib.com/test/wildpokeapp.html

-- Completely harmless compiler extension that makes some of the code cleaner
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Libraries for working with the API
import Data.Aeson
import Network.HTTP               (simpleHTTP, getResponseBody, getRequest)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Vector                (toList)

-- Imports for error handling
import Data.Maybe                 (fromMaybe)
import Control.Applicative        ((<$>),      (<*>))
import Control.Monad              (mzero)

-- Imported to be able to wait in the program
import Control.Concurrent         (threadDelay)

-- Libraries for working with randomness
import System.Random              (newStdGen,  randomRIO)
import System.Random.Shuffle      (shuffle')

-- Handy utility functions for doing random stuff on lists in the IO monad
shuffleIO :: [a] -> IO [a]
shuffleIO xs = shuffle' xs (length xs) <$> newStdGen
chooseIO :: [a] -> IO a
chooseIO xs = head <$> shuffleIO xs



-- We define an object for pokemon
data Pokemon = Pokemon {
    name  :: String,
    moves :: MoveList,
    atk   :: Integer,
    spAtk :: Integer
}

-- Decode JSON to Pokemon following this rule
instance FromJSON Pokemon where
  parseJSON (Object v) = Pokemon <$> v .: "name"   <*> v .: "moves"
                                 <*> v .: "attack" <*> v .: "sp_atk"
  parseJSON _ = mzero



-- The list of moves in a pokemon can either be a list of
-- concrete moves, or a list of potential moves that might
-- need to be downloaded first.
data MoveList = Moves [Move] | AvailableMoves [IO Move]

-- Decode JSON to a list of (potential) moves according to
-- this rule. This fetches the API URI for each move and
-- stores a download function in the list of potential
-- moves. When you want a move, just run the download
-- function.
instance FromJSON MoveList where
  parseJSON (Array v) = AvailableMoves <$> getterFunctions
    where getterFunctions = map apiRequest <$> mapM uri (toList v)
          uri (Object o) = o .: "resource_uri"
          uri _ = mzero
  parseJSON _ = mzero



-- A type for concrete moves
data Move = Move { movename :: String , power :: Integer }

-- Decode JSON to Moves following this rule
instance FromJSON Move where
  parseJSON (Object v) = Move <$> v .: "name" <*> v .: "power"
  parseJSON _ = mzero


-- A short helper function for making request URIs
makeRequest :: String -> Int -> String
makeRequest kind number = "/api/v1/" ++ kind ++ "/" ++ show number ++ "/"

-- Perform a request on the API and return the appropriate object
-- (this will be a Pokemon or a Move depending on circumstances)
apiRequest :: FromJSON a => String -> IO a
apiRequest uri = do
  let request = getRequest $ "http://viclib.com/pokemon?query=" ++ uri
  jsondata <- getResponseBody <$> simpleHTTP request
  either error id . eitherDecode . pack <$> jsondata


-- Download four random moves to a pokemon from the ones that are available
downloadMoves :: Pokemon -> IO Pokemon
downloadMoves pkmn@(Pokemon { moves = AvailableMoves getters }) = do
  moves <- sequence =<< take 4 <$> shuffleIO getters
  return pkmn { moves = Moves moves }

-- If the moves are already downloaded, do nothing.
downloadMoves pkmn = return pkmn


main :: IO ()
main = do
  -- Get a random #, hit the API to get the corresponding
  -- pokemon and download the moves for it
  pkmn <- downloadMoves =<< apiRequest =<< makeRequest "pokemon" <$> randomRIO (1,150)

  -- Enter battle
  putStrLn $ "Wild " ++ name pkmn ++ " appeared!"
  battle pkmn 200 0


battle :: Pokemon -> Int -> Int -> IO ()
battle pkmn hp moveCount =
  if hp <= 0
    then putStrLn $ name pkmn ++ " defeated you in " ++ show moveCount ++ " moves."
    else do
      putStrLn $ "You have " ++ show hp ++ " hp!"

      -- Pick a random move
      move <- case moves pkmn of Moves has -> chooseIO has
      -- Decide how much damage it does
      dmg <- randomRIO (0, floor $ fromInteger (power move * (atk pkmn + spAtk pkmn))/300)

      putStrLn $ name pkmn ++ " attacked you with " ++ movename move ++ " dealing " ++ show dmg ++ " damage!"

      -- Wait for a bit...
      threadDelay 4000000

      -- ...then repeat
      battle pkmn (hp - dmg) (succ moveCount)

