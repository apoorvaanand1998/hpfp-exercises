module Cipher where

import Data.Char


rot :: Int -> Char -> Char
rot x c
  | elem c ['a'..'z'] = chr (((ord c + x - ord 'a') `mod` 26) + ord 'a')
  | elem c ['A'..'Z'] = chr (((ord c + x - ord 'A') `mod` 26) + ord 'A')
  | otherwise = c

vigHelper1 :: String -> [Int]
vigHelper1 x = [if isUpper c
               then (ord c - ord 'A')
               else (ord c - ord 'a') | c <- x]

vigHelper2 :: String -> [Int] -> Int -> String
vigHelper2 msg key counter
  | msg == "" = ""              
  | otherwise =
      (rot
        (key !! counter)
        (head msg)) : (vigHelper2
                        (tail msg)
                        key
                        (if (isAlpha (head msg))
                         then ((counter+1) `mod` (length key))
                         else counter))

vigenere :: String -> String -> String
vigenere msg key = vigHelper2 msg (vigHelper1 key) 0

unVigenere :: String -> String -> String
unVigenere cipher key = vigHelper2 cipher (map negate (vigHelper1 key)) 0
