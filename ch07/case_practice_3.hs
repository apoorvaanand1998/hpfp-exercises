nums x = case compare x 0 of
  LT -> -1
  EQ -> 0
  GT -> 1
