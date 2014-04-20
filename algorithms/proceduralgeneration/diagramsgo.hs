
-- Go board using the Haskell Diagrams library

moves = ["d4", "f3", "h3"]


example :: Diagram B R2
example = board # showOrigin


board = stones `atop` grid


stones = position (zip (map parsecoord moves)
                       (cycle [circle 0.45 # lw 0.1 # fc black,
                               circle 0.45 # lw 0.1 # fc white]))

parsecoord (c:cs) =
  p2 (fromIntegral $ ord c - ord 'a',
      read cs - 1)

grid = vlines `atop` hlines

vlines = position (zip [p2 (x,0) | x <- [1..19]] (repeat vline))
hlines = position (zip [p2 (0,x) | x <- [1..19]] (repeat hline))

vline = vrule 18 # lw 0.1 # translateY 9 # translateX (-1)
hline = hrule 18 # lw 0.1 # translateX 9 # translateY (-1)
