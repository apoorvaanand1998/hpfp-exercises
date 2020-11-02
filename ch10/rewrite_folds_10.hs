import Data.List

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f ls = foldl' (\x y -> case f x y of
                              LT -> y
                              EQ -> y
                              GT -> x) (head ls) ls
