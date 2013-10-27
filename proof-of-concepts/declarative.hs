-- massFilter is a filter-like function taking as arguments however many
-- predicaes you would like, and partitions the list into as many pieces
-- as there are predicates -- while only traversing the list once.
-- This function only needs to be written once, as it is very general.
--
-- If you're into parallelism, map could be replaced with a parallel
-- implementation of some kind which distributes the work over several
-- cores.

massFilter :: [(a -> Bool)] -> [a] -> [[a]]
massFilter predicates []     = map (const []) predicates
massFilter predicates (x:xs) =
  zipWith (\p r -> if p x then x:r else r) predicates results
  where results = massFilter predicates xs


-- Request returns two things:
--  1. the sum of all elements equal to 25
--  2. all elements larger than 34

request arr =
  (sum $ parts !! 0, parts !! 1)
  where parts = massFilter [(== 25), (> 34)] arr