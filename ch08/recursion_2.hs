sumTill :: (Eq a, Num a) => a -> a
sumTill 0 = 0
sumTill n = n + sumTill (n-1)


sumTill' :: (Eq a, Num a) => a -> a
sumTill' n = go n 0
  where go n result
          | n == 0 = result
          | otherwise = go (n - 1) (result + n)
