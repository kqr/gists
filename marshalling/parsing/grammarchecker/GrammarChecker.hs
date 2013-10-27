
module GrammarChecker (Option(..), GrammaticalUnits(..), checkGrammar) where

import Data.Map (Map, (!))
import Data.List (stripPrefix)

-- En Token är det som i BNF skulle vara inom <>
-- En Literal är det som i BNF skulle vara inom ""
--
-- testliteral kontrollerar om det finns en Literal (av flera möjliga)
--     i början av kodsträngen
-- testtoken kontrollerar om det finns en Token (av en flera möjliga)
--     i början av kodsträngen
--
-- testgrammar tar vad som skulle vara en hel rad i BNF och kontrollerar
--     om den finns i början av kodsträngen

type Literal = String
type Token = String
type Code = String

data GrammaticalUnits = Tokens Option [Token]
                      | Literals Option [Literal]
data Option = Mandatory | Optional
optional (Tokens Optional _) = True
optional (Literals Optional _) = True
optional _ = False


testliteral :: [Literal] -> Code -> (Bool, Code)
testliteral [] code = (False, code)
testliteral (literal:literals) code =
  case stripPrefix literal code of
       Just new -> (True, new)
       Nothing -> testliteral literals code

testtoken :: Map Token [GrammaticalUnits] -> Code -> [Token] -> (Bool, Code)
testtoken _ [] _ = (True, [])
testtoken _ code [] = (False, code)
testtoken syntaxmap code (t:tokens) =
  let (yn, new) = testgrammar syntaxmap code $ syntaxmap ! t
  in  if yn
         then (True, new)
         else testtoken syntaxmap code tokens

testgrammar :: Map Token [GrammaticalUnits] -> Code -> [GrammaticalUnits] -> (Bool, Code)
testgrammar _ [] [] = (True, [])
testgrammar _ code [] = (True, code)
testgrammar syntaxmap code (first:rest) =
  let (yn, new) = case first of
                       Tokens _ tokens -> testtoken syntaxmap code tokens
                       Literals _ literals -> testliteral literals code
  in  if yn
         then testgrammar syntaxmap new rest
         else if optional first
              then testgrammar syntaxmap code rest
              else (False, code)

checkGrammar :: Map Token [GrammaticalUnits] -> Code -> Token -> (Bool, Code)
checkGrammar syntaxmap code initial =
  let (yn, rest) = testgrammar syntaxmap code $ syntaxmap ! initial
  in  if not $ null rest
         then (False, rest)
         else (yn, rest)
