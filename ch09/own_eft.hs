oneForAll :: (Enum a, Ord a) => a -> a -> [a]
oneForAll start stop = go start stop []
  where go start stop result
          | stop < start = []
          | start == stop = reverse $ start : result
          | otherwise = go (succ start) stop (start : result)

eftBool :: Bool -> Bool -> [Bool]
eftBool = oneForAll

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = oneForAll

eftInt :: Int -> Int -> [Int]
eftInt = oneForAll

eftChar :: Char -> Char -> [Char]
eftChar = oneForAll
