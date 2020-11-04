--1.

module Jammin where

import Data.List

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

-- 2.
data JamJars = Jam { jamType :: Fruit, jarQty :: Int }
               deriving (Eq, Show, Ord)

-- 3.
-- The cardinality of Jam Jars is Infinite

-- 4. (modified)

-- 5.

row1 = Jam Peach 23
row2 = Jam Plum 44
row3 = Jam Apple 2323
row4 = Jam Blackberry 99
row5 = Jam Peach 42
row6 = Jam Apple 420
row7 = Jam Blackberry 69
row8 = Jam Plum 9
allJam = [row1, row2, row3, row4, row5, row6, row7, row8]

-- 6.

totJars :: [JamJars] -> Int
totJars x = sum $ map jarQty x

-- 7.

mostJam :: [JamJars] -> JamJars
mostJam = foldr (\(Jam p q) (Jam x y) ->
                   if (q > y)
                   then (Jam p q)
                   else (Jam x y)) (Jam Apple 0)

mostJam' :: [JamJars] -> JamJars
mostJam' = maximumBy (\(Jam _ q) (Jam _ y) ->
                       compare q y)

-- 8. Done

-- 9.

sortJars :: [JamJars] -> [JamJars]
sortJars = sortBy (\(Jam k _) (Jam k' _) ->
                     compare k k')

-- 10.

groupJars :: [JamJars] -> [[JamJars]]
groupJars = groupBy (\(Jam k _) (Jam k' _) ->
                       k == k') . sortJars
