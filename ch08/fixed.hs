data DividedResult =
  Result Integer
  | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy x y = go (abs(x)) (abs(y)) 0
  where go x' y' result
          | y' == 0 = DividedByZero
          | x' < y' = Result (result * sign)
          | otherwise = go (x' - y') y' (result + 1)
          where sign
                  | (((x < 0) && (y < 0)) ||
                     ((x > 0) && (y > 0))) = 1
                  | otherwise = (-1)

