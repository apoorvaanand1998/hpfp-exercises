import Data.List

-- 1.

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe nt = Just nt

replaceThe :: String -> String
replaceThe s = concat $ intersperse " " $ f $ words s
  where f [] = []
        f (x : xs) = case (notThe x) of
          Nothing -> "a" : f xs
          Just nt -> nt : f xs

-- 2.

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where go [] result = result
        go (x : []) result = result
        go (x : xs) result = case (notThe x) of
          Nothing -> if elem (head (head xs)) "aeiou"
                     then go xs (result+1)
                     else go xs result
          Just nt -> go xs result

-- 3.

countVowels :: String -> Integer
countVowels = fromIntegral. length . filter isVowel
  where isVowel x = elem x "aeiou"
