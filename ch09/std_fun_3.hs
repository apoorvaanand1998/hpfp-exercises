myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = e == x || myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (e==)
