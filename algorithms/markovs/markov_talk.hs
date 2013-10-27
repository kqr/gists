
module Main where

import Prelude hiding (words, drop, length, unwords, lines, null)
import qualified Data.List as List (null, length)

import Data.ByteString.Char8 (ByteString, breakSubstring, words, unwords, pack, unpack, lines, drop, length, null)
import qualified Data.ByteString.Char8 as BS (snoc, readFile)

import Data.Sequence (Seq, (<|), index)
import qualified Data.Sequence as Seq (null, length, fromList, empty)

import Control.Monad (liftM)
import System.Random (randomRIO)


-- |choice returns a single randomly selected element out of a list.
choice :: Seq a -> IO a
choice xs = do
  r <- randomRIO (0, Seq.length xs - 1)
  return $ xs `index` r


-- |nextwords searches the corpus for the string specified, and returns
-- all the following words in a list.
nextwords :: ByteString -> [ByteString] -> Seq ByteString -> Seq ByteString
nextwords _ [] result = result
nextwords searchwords (line:corpus) result =
  let rest = snd $ breakSubstring searchwords line
  in  if null rest
         then nextwords searchwords corpus result
         else let word = head . words . drop (length searchwords) $ rest
              in  nextwords searchwords corpus (word <| result)


-- |fill_sentence starts out with an (almost) empty sentence and puts in
-- additional words by choosing randomly from nextwords.
fill_sentence :: [ByteString] -> Int -> [ByteString] -> IO [ByteString]
fill_sentence corpus pickiness sentence
  | List.length sentence > 15 = return $ reverse sentence
  | otherwise =
      let appendspace bs = BS.snoc bs ' '
          altwords = nextwords (appendspace . unwords . reverse . take pickiness $ sentence) corpus Seq.empty
      in  if Seq.null altwords
          then return $ reverse sentence
          else do
            word <- choice altwords
            fill_sentence corpus pickiness $ word : sentence


-- |markov_talk sets up the beginning of a sentence and then invokes
-- fill_sentence to fill the sentence up using randomly selected words.
markov_talk :: Int -> IO String
markov_talk pickiness = do
  corpus <- liftM lines $ BS.readFile "#adun.se.log"
  line <- choice $ Seq.fromList corpus
  let message = words . drop 2 . snd $ breakSubstring (pack "> ") line
  if List.null message
     then return ""
     else do
         let seed = reverse . take pickiness $ message
         sentence <- fill_sentence corpus pickiness seed
         return . unpack . unwords $ sentence


main :: IO ()
main = do
  markov_talk 3 >>= putStrLn
