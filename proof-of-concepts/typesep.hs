{-# LANGUAGE GADTs, StandaloneDeriving #-}


-- Separating different instances on the type level. This is a great
-- solution to a problem I don't remember what it was. I hope it comes
-- back to me in time because I really want to write about it.


data Volvo
data Ford
data Lotus

class Make m
instance Make Volvo
instance Make Ford
instance Make Lotus

data Vehicle m where
  Car :: Make m => String -> Int -> Vehicle m

deriving instance Show (Vehicle m)