newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs = if ((length xs - nVowels xs) < nVowels xs)
            then Nothing
            else Just (Word' xs)
  where nVowels = length . filter (\x -> elem x vowels)
