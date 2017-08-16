
import Data.Maybe (fromJust)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Control.Monad.State

prepare :: State (HashMap String Double) ()
prepare = do                                              -- def prepare(d):
  put M.empty                                             --     d.clear()
  modify (M.insert "a" 10.33)                             --     d['a'] = 10.33
  modify (M.insert "c" 20.23)                             --     d['c'] = 20.23
  one <- gets (M.lookup "a")                              --     one = d.get('a')
  two <- gets (M.lookup "c")                              --     two = d.get('c')
  let val = fromJust one + fromJust two                   --     val = one + two
  modify (M.insert "item" val)                            --     d['item'] = val
  return ()                                               --     return


method :: String -> State (HashMap String Double) String
method msg = do                                           -- def method(d, msg):
  prepare                                                 --     prepare(d)
  item <- gets (M.lookup "item")                          --     item = d.get('item')
  return (msg ++ show item)                               --     return msg + repr(item)
  

main :: IO ()
main = do                                                 -- def main():
  putStrLn "Enter a message:"                             --     print('Enter a message:')
  text <- getLine                                         --     text = input()
  let res = evalState (method text) M.empty               --     res = method(dict(), text)
  putStrLn ("Result: " ++ res)                            --     print('Result: ' + res)

