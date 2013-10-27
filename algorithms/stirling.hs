-- The Haskell prototype of stirling.c, derived from the spreadsheet prototype http://i.solidfiles.net/c44b894db1.png

data KGen = KGen Int [Integer] deriving Show

nextgen :: KGen -> KGen
nextgen (KGen n prevgen) =
  let nextgen = zipWith3 (\i k k_1 -> i*k + k_1) [0..] prevgen (0:prevgen)
  in  KGen (n+1) nextgen


stirling n k =
  case iterate nextgen (KGen 0 (1:replicate (k) 0)) !! n of
    KGen _ gen -> last gen
 
