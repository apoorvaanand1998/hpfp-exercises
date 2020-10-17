module BuildingFunctions2 where

exclaim :: String -> String
exclaim s = s ++ "!"

fifth :: String -> Char
fifth s = s !! 4

drop9 :: String -> String
drop9 s = drop 9 s
