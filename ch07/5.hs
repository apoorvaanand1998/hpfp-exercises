roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main = do
  print (roundTrip 4)
