import           Control.Monad (unless)
import           Evaluators
import           System.IO

main :: IO ()
main = do
  input <- read'

  unless (input == ":quit")
       $ print' (eval' input)
       >> main


read' :: IO String
read' = putStr "REPL> "
      >> hFlush stdout
      >> getLine

eval' :: String -> String
eval' input = input
  -- Try the following ones from `Evaluators.hs`:
  -- capitalizer input
  -- simpleCalc input
  -- emojifyer input

print' :: String -> IO ()
print' = putStr
