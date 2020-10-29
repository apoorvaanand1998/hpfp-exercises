myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x : []) = x
myMaximumBy f (x : xs) =
  case f x (myMaximumBy f xs) of
    LT -> myMaximumBy f xs
    EQ -> myMaximumBy f xs
    GT -> x
