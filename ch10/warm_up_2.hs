seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
  (length (words x))

-- Average number of letters per word
