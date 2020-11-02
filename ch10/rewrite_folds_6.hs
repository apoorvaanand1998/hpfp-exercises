myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a
                            then a : b
                            else b) []
