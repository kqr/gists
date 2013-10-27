module Main where

import Data.List
import Data.List.Split
import Control.Monad

-- A definition that really should be in the standard
-- library but seems to not be...
forEachLine f = interact $ unlines . map f . lines


main = forEachLine $ show . parse

parse = toString . sequence . fromString

fromString = map (splitOn "|") . splitOn "."
toString = map (intercalate ".")

{-
  Conforms to the specification

  "a.b.c.d"   -> ["a.b.c.d"]
  "a|b.c.d"   -> ["a.c.d", "b.c.d"]
  "a.b.c|d"   -> ["a.b.c", "a.b.d"]
  "a|b.c|d.e" -> ["a.c.e", "a.d.e", "b.c.e", "b.d.e"]
  "a|b|c.d"   -> ["a.d", "b.d", "c.d"]
  "a.chequered|checked.pattern" -> ["a.chequered.pattern", "a.checked.pattern"]
-}
