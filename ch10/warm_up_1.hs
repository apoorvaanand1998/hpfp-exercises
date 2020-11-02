stops = "pbtdkg"
vowels = "aeiou"

nouns = ["donkey", "Anand", "guava"]
verbs = ["run", "dance", "eat", "sleep", "drink", "die"]

threeComb :: [a] -> [b] -> [(a, b, a)]
threeComb xs ys = [(x, y, x') | x <- xs, y <- ys, x' <- xs]

a = threeComb stops vowels
c = threeComb nouns verbs

b = filter (\(x, y, z) -> x == 'p') a
