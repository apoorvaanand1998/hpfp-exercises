myOr :: [Bool] -> Bool
myOr [] = False
myOr (b : bs) = b || myOr bs
