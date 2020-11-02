-- 1
fibs20 = take 20 $ 1 : (scanl (+) 1 fibs20)

-- 2
fibsUnder100 = takeWhile (< 100) $ 1 : (scanl (+) 1 fibsUnder100)

-- 3
fact10 = take 10 $ scanl (*) 1 [1..]
