module Cipher where

import Data.Char

rot :: Int -> Char -> Char
rot x c
  | elem c ['a'..'z'] = chr (((ord c + x - ord 'a') `rem` 26) + ord 'a')
  | elem c ['A'..'Z'] = chr (((ord c + x - ord 'A') `rem` 26) + ord 'A')
  | otherwise = c
  
caesar :: Int -> String -> String
caesar _ [] = []
caesar r xs = map (rot r) xs

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar r xs = map (rot (-r)) xs
