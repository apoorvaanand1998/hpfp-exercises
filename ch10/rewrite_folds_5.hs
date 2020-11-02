myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []
