import Data.Char

capHead :: String -> Char
capHead = toUpper . head
