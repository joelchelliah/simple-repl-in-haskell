import           Control.Monad
import           Evaluators
import           System.IO

main :: IO ()
main = do
  input <- read'

  unless (input == ":quit")
       $ eval' input
     >>= print'
      >> main

read' :: IO String
read' = putStr "REPL> "
      >> hFlush System.IO.stdout
      >> getLine

eval' :: String -> IO String
eval' =
  return
  -- Try the following ones from `Evaluators.hs`:
  -- return . capitalizer
  -- return . simpleCalc
  -- return . emojifyer

print' :: String -> IO ()
print' = putStrLn
