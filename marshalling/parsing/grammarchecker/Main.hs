
module Main where

import qualified Data.Map as Map

import GrammarChecker


-- Syntaxtabellen är implementerad med Data.Map
-- Denna tabell motsvarar i BNF:
--
--     <program> ::= <whitespace> | <expression> . [<program>]
--     <whitespace> ::= " " | "\t" | "\n" | "\r" . [<whitespace>]
--     <expression> ::= <primitive>
--     <primitive> ::= "()"
--
-- Det är naturligtvis bara början på en riktig lisp och mycket återstår.
 
dopeysyntax = Map.fromList $
  [("program",    [Tokens   Mandatory ["whitespace", "expression"]
                  ,Tokens   Optional  ["program"]])
  ,("whitespace", [Literals Mandatory [" ", "\t", "\n", "\r"]
                  ,Tokens   Optional  ["whitespace"]])
  ,("expression", [Tokens   Mandatory ["primitive"]])
  ,("primitive",  [Literals Mandatory ["()"]])]


{- Kod testas:

*Main> checkGrammar dopeysyntax "() ()) ()" "program"
(False,") ()")
*Main> checkGrammar dopeysyntax "() ()()()" "program"
(True,"")
*Main> checkGrammar dopeysyntax "()       " "program"
(True,"")
*Main> checkGrammar dopeysyntax "      ()" "program"
(True,"")
*Main> checkGrammar dopeysyntax "" "program"
(True,"")

-}
