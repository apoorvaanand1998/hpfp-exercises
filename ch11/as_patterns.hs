import Data.Char
-- 1.

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xl@(x : xs) yl@(y : ys)
  | x == y = isSubsequenceOf xs yl
  | otherwise = isSubsequenceOf xl ys

-- 2.

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords sentence = f (words sentence)
  where f wordList = map g wordList
        g xl@(x : xs) = (xl, toUpper x : xs)

-- screw this one   
