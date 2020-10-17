module BuildingFunctions5 where

rvrs :: String -> String
rvrs s = concat [(drop 9 s), " ", (take 2 (drop 6 s)), " ", (take 5 s)]
-- horrible
