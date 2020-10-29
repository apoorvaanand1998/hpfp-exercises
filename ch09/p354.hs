-- 1

f1 xs = filter (\x -> x `rem` 3 == 0) xs
-- xs = [1..30]

-- 2

f2 = length . f1

-- 3

myFilter = filter (\x -> not $ elem x ["the", "a", "an"]) . words
