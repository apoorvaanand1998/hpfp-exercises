import Data.Char
import Data.List
-- 1.

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x : xs) = toUpper x : xs

-- 2.

mySplit :: Char -> (String -> [String])
mySplit c = \s -> go s []
  where go s result
          | s == "" = reverse result
          | otherwise = go (f s) (g s : result)
          where f = dropWhile (== c) . dropWhile (/= c)
                g = takeWhile (/= c)
-- taken from a previous exercise
-- probably exists in some library/package

capFirstWord :: String -> String
capFirstWord [] = []
capFirstWord (x : xs) = if (isAlpha x)
                        then (toUpper x : xs)
                        else (x : capFirstWord xs)
                             
-- capitalizeParagraph :: String -> String
capitalizeParagraph paragraph =
  concat $ intersperse "." $ map capFirstWord $ mySplit '.' paragraph
