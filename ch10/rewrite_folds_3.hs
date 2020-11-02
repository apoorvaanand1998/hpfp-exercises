myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\a b -> b || a == e) False
