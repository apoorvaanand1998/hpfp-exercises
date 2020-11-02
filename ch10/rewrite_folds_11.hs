import Data.List

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f ls = foldl' (\x y -> case f x y of
                              LT -> x
                              EQ -> x
                              GT -> y) (head ls) ls
