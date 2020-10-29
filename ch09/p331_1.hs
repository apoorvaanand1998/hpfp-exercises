myWords :: String -> [String]
myWords sentence = go sentence []
  where go sentence result
          | sentence == "" = reverse result
          | otherwise = go (f sentence) (g sentence : result)
          where f = dropWhile (== ' ') . dropWhile (/= ' ')
                g = takeWhile (/= ' ')

myWords' :: String -> [String]
myWords' sentence =
  if (sentence == "") then []
  else ((g sentence) : (myWords' (f sentence)))
  where f = dropWhile (== ' ') . dropWhile (/= ' ')
        g = takeWhile (/= ' ')
       
