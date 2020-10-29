myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x : []) = x
myMinimumBy f (x : xs) =
  case f x (myMinimumBy f xs) of
    LT -> x
    EQ -> x
    GT -> myMinimumBy f xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x : []) = x
myMaximumBy f (x : xs) =
  case f x (myMaximumBy f xs) of
    LT -> myMaximumBy f xs
    EQ -> myMaximumBy f xs
    GT -> x

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

-- https://stackoverflow.com/questions/45304154/inconsistency-between-minimumby-and-maximumby
