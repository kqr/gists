import Data.List

{-
  Logic puzzle borrowed from http://www.youtube.com/watch?v=hEOVcLAPRG8#t=27m0s
  and specified as

    1. Adam does not live on the top floor.
    2. Bill does not live on the bottom floor.
    3. Cora does not live on either the top or the bottom floor.
    4. Dale lives on a higher floor than does Bill.
    5. Erin does not live on a floor adjacent to Cora's.
    6. Cora does not live on a floor adjacent to Bill's.

  Encoded in a similar way as it would be in Prolog. This is not good code,
  in the sense that it only uses top-level definitions of goals and a very
  dynamic approach which isn't very well checked statically, but that's just
  the way Prolog is done so I've tried to imitate it.

  A real problem solver like this in Haskell would probably use a better DSL
  with more static guarantees and neater syntax and stuff. Maybe that exists.
  I haven't checked and I should be doing more important things so I won't.

  Does indeed give the correct answer:

      *Main> puzzle
      [["Dale","Cora","Adam","Bill","Erin"]]

  If one would relax the requirements (just commented out two random goals)
  it gives several possible answers, lazily!

      [["Dale","Bill","Adam","Cora","Erin"]
      ,["Adam","Cora","Dale","Bill","Erin"]
      ,["Dale","Cora","Adam","Bill","Erin"]
      ,["Dale","Cora","Erin","Bill","Adam"]
      ,["Erin","Cora","Dale","Bill","Adam"]
      ,["Dale","Bill","Erin","Cora","Adam"]]

   Since it generates answers lazily, they could be asked for one by one
   instead -- just like in Prolog!

-}

puzzle = filter valid $ permutations ["Adam", "Bill", "Cora", "Dale", "Erin"]

valid (attempt@[top, n4, n3, n2, bottom]) = and [
  top    /= "Adam",
  bottom /= "Bill",
  top    /= "Cora",
  bottom /= "Cora",
  higher "Dale" "Bill" attempt,
  not $ adjacent "Erin" "Cora" attempt,
  not $ adjacent "Cora" "Bill" attempt]

higher a b (x:xs)
  | x == a && b `elem` xs = True
  | otherwise             = higher a b xs
higher _ _ _              = False

adjacent a b (x:y:zs)
  | a == x && b == y      = True
  | b == x && a == y      = True
  | otherwise             = adjacent a b (y:zs)
adjacent _ _ _            = False

 