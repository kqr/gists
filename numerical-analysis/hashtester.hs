{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List (foldl')
import Control.Applicative ((<$>))
import Data.Set (Set, size, empty, insert)

import Data.Char (ord)
import Data.Bits (shiftL, xor)




---------------------------------------------------
-- Parameters that might be interesting to change
fn = "cropus.txt"
hashSize = 2
usedHasher = xorHasher hashSize
---------------------------------------------------




---------------------------------------------------
-- Space for so much activities! (hash functions)





-- Reference word-wise xor hash function. Reaaaally crummy.
xorHasher bytes = fst . foldl' combine (0, 0)
  where combine (acc, curPos) c = (shiftL (ord c) (8*curPos) `xor` acc, succ curPos `mod` bytes)
---------------------------------------------------





main = do
  ws <- words <$> readFile fn
  let !(u, h) = countCollisions ws
      collisions = u - h
      utilisation = fromIntegral h/2^(hashSize*8)

  putStrLn $ "Unique words: " ++ show u
  putStrLn $ "Collisions: " ++ show collisions
  putStrLn $ "Hash space utilised: " ++ show utilisation


countCollisions ws = f ws empty empty
  where f []      unique  hashed = (size unique, size hashed)
        f (w:ws) !unique !hashed = f ws (insert w unique) (insert (usedHasher w) hashed)

 