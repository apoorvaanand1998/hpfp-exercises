module IsPal where


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
