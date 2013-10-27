module Main where
import Prelude hiding (map)
import Data.List (foldl')
import Data.Map (Map, map, fromDistinctAscList, toAscList, insertWith, empty)
import Data.Tuple (swap)
import Control.Applicative ((<$>))

-- tiny, general utility functions that almost should
-- have been in the libraries to begin with.
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
unwind :: (a, [b]) -> [(a, b)]
unwind (a, b) = ((,) a) <$> b
aggregate :: (Ord k) => Map k [a] -> (k, a) -> Map k [a]
aggregate m (k, a) = insertWith (++) k [a] m


data Corpus = Corpus (Map Int String) (Map String [Int])


main = do
  -- Read the corpus file and chunk it into a Corpus
  corpus <- chunk <$> readFile "corpus.txt"
  -- So far, only chunks up a Corpus, doesn't generate anything.
  sentence <- corpus `generate` 16
  putStrLn sentence


chunk :: String -> Corpus
chunk str = Corpus chunks dictionary
  where
    chunks = fromDistinctAscList . enumerate $ lines str
    dictionary = foldl' aggregate empty $ swap <$> chunklist
    chunklist = concatMap unwind . toAscList $ map words chunks


generate :: Corpus -> Int -> IO String
generate = undefined