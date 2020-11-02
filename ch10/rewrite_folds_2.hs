myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False
