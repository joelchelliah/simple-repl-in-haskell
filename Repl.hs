import           Control.Monad     (unless)
import           EvaluatorExamples
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
eval' input =
  input
  -- Try the following ones from `EvaluatorExamples.hs`:
  -- capitalizer input
  -- simpleCalc input
  -- emojiFinder input


print' :: String -> IO ()
print' = putStrLn
