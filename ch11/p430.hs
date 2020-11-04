{-# LANGUAGE FlexibleInstances #-}
-- 1.

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Integer where -- changed from Int to Integer
  tooMany n = n > 42
-- So when Num type defaults to Integer
-- This is the part that'll be called
  
instance TooMany (Int, String) where
  tooMany (x, y) = x > 50 && y > "aa"

-- 2.

instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 42

-- 3.

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
