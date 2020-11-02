import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1

fddHelper (DbDate a) b = a : b
fddHelper _ b = b

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbi = foldr fddHelper [] dbi

-- 2

fdnHelper (DbNumber a) b = a : b
fdnHelper _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr fdnHelper []

-- 3

mrHelper (DbDate a) b
  | a > b = a
  | otherwise = b
mrHelper _ b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr mrHelper (UTCTime
                             (fromGregorian 1900 5 1)
                             (secondsToDiffTime 34123))

-- 4

sdbHelper (DbNumber a) b = a + b
sdbHelper _ b = b

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sdbHelper 0

-- 5

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) /
           fromIntegral (length $ filterDbNumber db)

adbHelper (DbNumber a) (s, l) = (s + a, l + 1)
adbHelper _ (s, l) = (s, l)
  
avgDb' :: [DatabaseItem] -> Double
avgDb' db = let (s, l) = foldr adbHelper (0, 0) db in fromIntegral s / l
