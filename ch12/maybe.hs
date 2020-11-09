-- 1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- 2.

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe a x = mayybee a id x

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes = concat. map listToMaybe . map []
