module Evol where

import Control.Monad (replicateM)

import MolSeq
import Profile

class Evol a where
  distance :: a -> a -> Double
  evolType :: a -> SeqType
  evolName :: a -> String

  distanceMatrix :: [a] -> [(String, String, Double)]
  distanceMatrix evols = do
    [e1, e2] <- replicateM 2 evols
    return (evolName e1, evolName e2, distance e1 e2)


instance Evol MolSeq where
  distance = seqDistance
  evolType = seqType
  evolName = seqName


instance Evol Profile where
  distance = profileDistance
  evolType = profileType
  evolName = profileName
 