module MolSeq where

import Data.List (genericLength)


-- Exercise 1: The MolSeq data type.
-- (Also a little exercise 3: Create functions that return the name and sequence of a MolSeq value.)

data SeqType = DNA | Protein deriving (Show, Eq)
data MolSeq = MolSeq { seqType :: SeqType
                     , seqName :: String
                     , seqSequence :: String
                     } deriving Show


alphabet :: SeqType -> String
alphabet DNA = "ACGT"
alphabet Protein = "ACDEFGHIKLMNPQRSTVWXY"


-- Exercise 2: The seq builder function. Assuming sequences only
-- consisting of A, C, G or T are DNA sequences.
string2seq name seq = MolSeq typeguess name seq
  where typeguess = if all (`elem` alphabet DNA) seq then DNA else Protein


-- The rest of exercise 3: Create a function that returns the length of the sequence of a MolSeq.
seqLength = length . seqSequence


-- Exercise 4: The seqDistance function figuring out d_{a,b} for two MolSeqs.
-- A guard to make sure the sequences being compared are of the same type and equally long.
seqDistance (MolSeq sta _ sqa) (MolSeq stb _ sqb) | sta == stb && length sqa == length sqb =

  -- The Jukes-Cantor and Poisson model expressed in terms of alpha and the magic alphabet size
  -- determined constant.
  if alpha > magic_constant - 0.01
     then 2*magic_constant + 1.8
     else negate $ magic_constant * log (1 - alpha/magic_constant)

  where alpha = hamming sqa sqb / genericLength sqa
        magic_constant = 1 - (recip . genericLength . alphabet $ sta)

seqDistance _ _ = error "seqDistance: Cannot compare sequences of different type or length!"


-- Get the hamming distance of two strings. (Undefined behaviour
-- with strings of differing lengths.)
hamming = genericLength . filter (uncurry (/=)) .: zip
  where (.:) = (.).(.); infixr 8 .:
 