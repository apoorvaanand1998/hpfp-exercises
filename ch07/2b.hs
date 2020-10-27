foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b = x
  | otherwise = y
