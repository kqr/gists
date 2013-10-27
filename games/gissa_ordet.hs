
module Main where

import Data.Char (toUpper)
import Data.Bits (complement, xor)
import System.Random (getStdGen, StdGen, randomR)
import Data.List (nub)
import qualified Data.ByteString as ByteString (readFile, map)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)

decrypt :: ByteString -> ByteString
decrypt = ByteString.map ((+256) . xor 0xa6 . complement)

cover :: String -> [Char] -> String
cover str letters = map (\c -> if c `elem` letters then c else '*') str

load_wordlist :: FilePath -> IO [String]
load_wordlist fn = do
  rawcontent <- ByteString.readFile fn
  return $ (filter (/= "") . lines . map toUpper . toString . decrypt) rawcontent

get_answer :: (String -> Bool) -> IO String
get_answer allowed = do
  input <- getLine >>= return . map toUpper
  if allowed input
     then return input
     else get_answer allowed

play :: [String] -> StdGen -> IO ()
play wordlist rgen =
  let (wordn, nextgen) = randomR (0, length wordlist - 1) rgen
      word = wordlist !! wordn
      in do
        game word []
        putStrLn $ "Ordet var " ++ word
        putStrLn $ "Vill du spela igen? [Y]es eller [N]o"
        input <- get_answer (`elem` ["Y", "N"])
        case input of
             "Y" -> play wordlist nextgen
             "N" -> return ()

game :: String -> [Char] -> IO ()
game word guessed
  | word == cover word guessed = putStrLn $ "Du vann!!!!!!!~~~"
  | length guessed > 10        = putStrLn $ "Du förlorade. :("
  | otherwise                  = do
    putStrLn $ "Avtäckt är " ++ cover word guessed ++ "!!"
    putStrLn $ "Gissat är " ++ guessed ++ "."
    putStrLn $ "Ny gissning?"
    input <- get_answer (\s -> length s == 1 || s == "QUIT")
    case input of
         "QUIT" -> return ()
         _ -> game word . reverse . nub . reverse $ head input : guessed
                  

main :: IO ()
main = do
  wordlist <- load_wordlist "../wordlist/ordlista_krypt.dat"
  rgen <- getStdGen
  play wordlist rgen

