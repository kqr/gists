module Main where
import Data.List (unfoldr)


floydTriangle =
  unfoldr (Just . nextFloyd) (1,1)
  where nextFloyd (number, quantity) =
          (take quantity [number..], (number+quantity, quantity+1))


main = do
  putStr "Depth? "
  depth <- read <$> getLine

  let finiteTriangle = take depth floydTriangle

  mapM_ (putStrLn . show) finiteTriangle