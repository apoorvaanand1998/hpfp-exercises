rMul :: Integral a => a -> a -> a
rMul x 0 = 0
rMul x y = x + rMul x (y-1)

rMul' :: Integral a => a -> a -> a
rMul' x y = go x y 0
  where go x y result
          | y == 0 = result
          | otherwise = go x (y - 1) (result + x)
