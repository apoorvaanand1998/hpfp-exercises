mySplit :: Char -> (String -> [String])
mySplit c = \s -> go s []
  where go s result
          | s == "" = reverse result
          | otherwise = go (f s) (g s : result)
          where f = dropWhile (== c) . dropWhile (/= c)
                g = takeWhile (/= c)

myLines = mySplit '\n'

myWords = mySplit ' '
