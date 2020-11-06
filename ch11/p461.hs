data Quantum = Yes
             | No
             | Both
             deriving (Show, Eq)

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

-- all true, no false

convert2 :: Quantum -> Bool
convert2 Yes = False
convert2 No = False
convert2 Both = False

-- all false, no true

convert3 :: Quantum -> Bool
convert3 Yes = False
convert3 No = True
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = True
convert5 No = True
convert5 Both = False

-- 1 false, 2 true

convert6 :: Quantum -> Bool
convert6 Yes = True
convert6 No = False
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = True
convert8 Both = False

-- 2 false, 1 true

-- Thus the formula b^a (2^3 = 8) holds
