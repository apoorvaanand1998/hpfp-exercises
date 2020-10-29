import Data.Char

capFirst :: String -> String
capFirst [] = []
capFirst (x : xs) = (toUpper x) : xs
