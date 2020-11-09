-- 1.

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) b = a : b
        f _ b = b

-- 2.

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right a) b = a : b
        f _ b = b

-- 3.

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' _ _ = Nothing

-- 5.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left a) = f1 a
either' _ f2 (Right b) = f2 b

-- 6.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (Just . f) x
