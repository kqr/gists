-- Conforms to
-- (0x44 or 0x22) and (0x32 or (not 0x39)) ==> 102

module Main where
import Data.Bits
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))
import Numeric (readHex)


type Operator = Expression -> Expression -> Expression
data Expression = Literal Int
                | Not Expression
                | And Expression Expression
                | Or Expression Expression
                | Xor Expression Expression
                deriving Show


compute :: Expression -> Int
compute exp = case exp of
  (Literal n) -> n
  (Not exp1) -> complement $ compute exp1
  (And exp1 exp2) -> compute exp1 .&. compute exp2
  (Or exp1 exp2) -> compute exp1 .|. compute exp2
  (Xor exp1 exp2) -> compute exp1 `xor` compute exp2


literal = string "0x" >> many1 hexDigit >>= return . Literal . fst . (!!0) . readHex
negation = string "not" >> spaces >> hardExpression >>= return . Not

binary = do
  exp1 <- hardExpression
  spaces
  op <- string "and" <|> string "or" <|> string "xor"
  spaces
  exp2 <- hardExpression
  return $ case op of
    "and" -> And exp1 exp2
    "or" -> Or exp1 exp2
    "xor" -> Xor exp1 exp2

parenthetical = do
  char '('
  expr <- expression
  char ')'
  return expr

hardExpression = literal <|> parenthetical

expression :: Parser Expression
expression = try binary <|> negation <|> hardExpression

main = do
  str <- getLine
  putStrLn $
    case parse expression "" str of
      Right expr -> show $ compute expr
      Left err -> show err
 