module Main where

import Prelude hiding (Nothing, Left, Right)
import Control.Concurrent
import System.IO.Unsafe
import UI.HSCurses.Curses

gameWidth = 40
gameHeight = 10
paddleHeight = 4



{-
---------------------- Data --------------------
-}

-- The input at any moment in time can be either nothing,
-- a direction in which to move the paddle, or a request
-- to quit the game.
data Input = Nothing | Movement Direction | Quit
data Direction = Up | Down

-- The game state consists of a ball and two paddles.
data GameState = Game Ball Paddle Paddle | Ended
data Paddle = Left Int | Right Int
-- A ball has four properties: x, y, xspeed and yspeed.
data Ball = Ball Int Int Int Int



{-
---------------------- Game engine --------------------
-}


-- This checks for game loss, collision with walls and
-- paddles, and otherwise just advances the ball
-- according to its speed.
stepBall :: GameState -> GameState
stepBall (Game ball left right)
  | x <= 0 || x >= gameWidth                  = Ended
  | y == 1                                    = stepBall (Game (Ball x (y+1) xs (-ys)) left right)
  | y == gameHeight                           = stepBall (Game (Ball x (y-1) xs (-ys)) left right)
  | x == lx && y `elem` [ly..ly+paddleHeight] = stepBall (Game (Ball (x+1) y (-xs) ys) left right)
  | x == rx && y `elem` [ry..ry+paddleHeight] = stepBall (Game (Ball (x-1) y (-xs) ys) left right)
  | otherwise                                 = Game (Ball (x+xs) (y+ys) xs ys) left right
  where
    (Ball x y xs ys) = ball
    lx        = 3+1
    (Left ly) = left
    rx         = gameWidth - 3 - 1
    (Right ry) = right

-- This checks which way the ball is moving and
-- imitates that at limited speed
stepAI :: GameState -> GameState
stepAI (Game ball (Left ly) right) =
  Game ball (Left $ constrain (ly + signum ys) 1 (gameHeight-paddleHeight)) right
  where
    (Ball _ _ _ ys) = ball
    constrain v a b
      | v < a     = a
      | v > b     = b
      | otherwise = v


-- Depending on what direction, move the paddles and
-- make sure they do not exit the court.
movePaddle :: Direction -> GameState -> GameState
movePaddle Up   (Game ball left (Right ry)) = Game ball left (Right $ max 1 (ry - 1))
movePaddle Down (Game ball left (Right ry)) = Game ball left (Right $ min (gameHeight-paddleHeight) (ry + 1))

-- Transform the state according to a single input.
transformSingle :: Input -> GameState -> GameState
transformSingle Nothing state = state
transformSingle (Movement Up) state = movePaddle Up state
transformSingle (Movement Down) state = movePaddle Down state
transformSingle Quit _ = Ended

-- Given an initial state and a list of inputs, return
-- a list of all the intermediate states until the game
-- ends when the state is transformed with the inputs.
-- Note that there is no base case for an empty input
-- list, because the input list should be considered
-- "infinitely long" as long as the game is running.
transform :: [Input] -> GameState -> [GameState]
transform (i:input) state =
  state : (case transformSingle i state of
             Ended    -> [Ended]
             newState -> transform input $ stepGame newState)
  where
    stepGame = stepBall . stepAI






{-
---------------------- Input/Output --------------------
-}


-- Given a (curses) window to draw on and a list of
-- game states, draw each state successively. Here,
-- too, the list of game states should be considered
-- infinite. When the game ends, it should end with
-- one of the game-ending game states, not abruptly
-- with the end of the list.
draw :: Window -> [GameState] -> IO ()
draw w (Ended:_) = return ()
draw w ((Game (Ball x y _ _) (Left ly) (Right ry)):states) = do
  wclear w

  mvWAddStr w 0 0 $ replicate gameWidth '-'
  mvWAddStr w (gameHeight+1) 0 $ replicate gameWidth '-'
  mvWAddStr w y x "o"
  drawPaddle 3 ly
  drawPaddle (gameWidth-3) ry

  refresh
  threadDelay 100000
  draw w states

  where
    drawPaddle x yoffset =
      mapM_ (\i -> mvWAddStr w i x "|") [yoffset..yoffset+paddleHeight]


-- Get an infinite sequence of keys from curses.
getKeys :: IO [Key]
getKeys = unsafeInterleaveIO $ do
  a <- fmap decodeKey getch
  as <- getKeys
  return $ a : as




{-
---------------------- Glue --------------------
-}


main = do
  -- Set a few options for curses
  initCurses
  w <- initScr
  raw True
  noDelay w True
  echo False

  -- Transform the infinite list of keys into an
  -- infinite list of inputs that makes sense for
  -- the game engine.
  inputs <- fmap (map toInternalInput) getKeys

  -- Record the game history (all the intermediary
  -- states) by transforming the initial state with
  -- the inputs.
  let centerx = gameWidth `div` 2
      centery = gameHeight `div` 2
      gameHistory = transform inputs $ Game (Ball centerx centery 1 1)
                                            (Left $ centery - paddleHeight `div` 2)
                                            (Right $ centery - paddleHeight `div` 2)

  -- Draw the entire history!
  draw w gameHistory

  -- Be nice and tell the user that there's no
  -- more game to be played
  wclear w
  mvWAddStr w 3 3 "Game Over! Press q to exit"
  waitForOkay

  -- Reset the terminal and quit curses
  endWin

  where
    toInternalInput :: Key -> Input
    toInternalInput (KeyChar 'u') = Movement Up
    toInternalInput (KeyChar 'e') = Movement Down
    toInternalInput (KeyChar 'q') = Quit
    toInternalInput _ = Nothing

    waitForOkay :: IO ()
    waitForOkay = do
      c <- fmap decodeKey getch
      case c of
        (KeyChar 'q') -> return ()
        _             -> waitForOkay


