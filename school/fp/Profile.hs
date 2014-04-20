module Profile where

import Data.List (sort, group, transpose, unionBy, genericLength)
import Data.Function (on)
import Control.Applicative ((<$>), (<*>))

import MolSeq

-- Exercise 1:
data Profile = Profile { profileType :: SeqType
                       , profileName :: String
                       , profileSeqs :: Int
                       , getMatrix :: [[Double]]
                       } deriving Show


type AssocList key value = [(key, value)]

makeProfileMatrix :: [MolSeq] -> [AssocList Char Int]
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix seqs = res
  where
    t = seqType (head seqs)
    n = length seqs
    -- Sets the frequency to 0 for each letter while creating an association list
    defaults = zip (alphabet t) $ replicate (length (alphabet t)) 0
    strs = map seqSequence seqs
    -- Compress a list of identical values by turning it into an (element, length) pair
    compress = (,) <$> head <*> length
    --     For a collection of letters at position n in every sequence,
    --       make a separate list for each character
    --       and compress it to a list of (character, occurrences) pairs
    -- The result is a list of the (character, occurrences) pairs for every
    -- position in the profile.
    tmp1 = map (map compress . group . sort) $ transpose strs
    -- Merge two association lists by their keys, retaining the values from the last one
    -- in case of collisions
    merge = unionBy ((==) `on` fst)
    -- Combine the results from the previous steps with the default value 0
    -- for letters that didn't occur. Order of arguments to unionBy is important,
    -- since otherwise the attained results would be thrown away.
    -- Then a sort is applied so that the resulting association list is in the right order.
    res = map sort . map (flip merge defaults) $ tmp1


-- Exercise 2:
fromMolSeqs :: [MolSeq] -> Profile
fromMolSeqs seqs@(MolSeq st tn _ : _) =
  Profile st tn (length seqs) $ normalise (makeProfileMatrix seqs)
  where
    -- Pick out the number from the association list and divide it by the
    -- number of sequences to create a profile matrix of Doubles
    normalise = map . map $ (/genericLength seqs) . fromIntegral . snd


-- Exercise 3:
profileDistance :: Profile -> Profile -> Double
-- For both arguments, get their profile matrix and flatten it. Take the difference
-- of the matrices element-wise and then sum said differences.
profileDistance = sum .: zipWith difference `on` concat . getMatrix
  where difference = abs .: subtract
        (.:) = (.).(.)



 