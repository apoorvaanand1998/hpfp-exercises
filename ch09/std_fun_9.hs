myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x : []) = x
myMinimumBy f (x : xs) =
  case f x (myMinimumBy f xs) of
    LT -> x
    EQ -> x
    GT -> myMinimumBy f xs
