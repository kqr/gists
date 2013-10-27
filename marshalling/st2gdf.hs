-- A converter between a custom graph format and GDF

module Main where


commas = foldr f [[]] 
  where f c l@(x:xs) | c == ','  = []:l
                     | otherwise = (c:x):xs

nodeify [] = []
nodeify (line:rest) =
  (node !! 0, node !! 1, node !! 2) : nodeify rest
  where
    node = map (\s -> if head s == ' ' then tail s else s) $ commas line

edgeify [] = []
edgeify (line:rest) =
  [(node, conn) | conn <- conns] ++ edgeify nodes rest
  where
    node  = head $ commas line
    conns = map (\s -> if head s == ' ' then tail s else s) . drop 3 $ commas line

undirect [] = []
undirect ((a, b):rest) =
  if (b, a) `elem` rest
     then undirect rest
     else (a, b) : undirect rest

main :: IO ()
main = do
  contents <- readFile "stationlist.txt"
  let nodes = nodeify $ lines contents
      edges = undirect . edgeify nodes $ lines contents
      nodesout = unlines [name ++ "," ++ weight ++ "," ++ label | (name, weight, label) <- nodes]
      edgesout = unlines [name1 ++ "," ++ name2 | (name1, name2) <- edges]

  writeFile "stationlist.gdf" $ nodesout ++ edgesout
  putStrLn "Done"


