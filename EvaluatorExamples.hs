module EvaluatorExamples where

import           Data.Char
import           Data.List

-- Some simple evaluator examples:


-- Capitalizes each word in the given input
capitalizer :: String -> String
capitalizer =
  unwords . map capitalize . words
  where capitalize (h:t) = toUpper h : (toLower <$> t)
        capitalize _     = []


-- Does really really simple calculations
simpleCalc :: String -> String
simpleCalc expr =
  case words expr of
    [x, "+", y] -> show $ read x + read y
    [x, "-", y] -> show $ read x - read y
    [x, "*", y] -> show $ read x * read y
    [x, "/", y] -> show $ read x / read y
    _           -> "That's too hard! :("


-- Finds emojis related to the given input
emojiFinder :: String -> String
emojiFinder expr
  | foundIn ["smile", "face"] = "😀  😃  😄  😁  😆  😅  😂"
  | foundIn ["car", "transport"] = "🚗  🚕  🚙  🏎  🚓  🚑"
  | foundIn ["cat", "meow", "purr"] = "😺  😸  😻  😼  🙀"
  | foundIn ["ball", "sport"] = "⚽️  🏀  🏈  ⚾️  🎾  🎱"
  | foundIn ["food"] = "🍏  🍎  🍐  🍊  🍋  🍌  🍉  🍇  🍓  🍒  🍑"
  | otherwise = "👾  ⁉️"
  where foundIn  = not . null . intersect keywords
        keywords = words expr
