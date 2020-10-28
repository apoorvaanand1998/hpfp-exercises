module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = go n []
  where go n l
          | n == 0 = l
          | otherwise = go (n `div` 10) ((n `mod` 10) : l)

wordNumber :: Int -> String
wordNumber n = if (n < 0) then ("minus-" ++ result) else result
  where result = concat $ intersperse "-" $ map digitToWord $ digits $ abs n
