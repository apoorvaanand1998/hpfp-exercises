foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y
