import Data.Char
import Data.List
import Data.Maybe -- to use fromJust (googled that)
import Data.Ord -- for comparing

type ReprSym = Char
type ActualSym = Char
type Presses = Int

rs :: [ReprSym]
rs = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '*', '0', '#']

as :: [[ActualSym]]
as = ["1", "abc2", "def3", "ghi4", "jkl5", "mno6", "pqrs7", "tuv8",
      "wxyz9", "^*", "+ 0", ".,#"]

-- 1.

data Phone = Phone [(ReprSym, [ActualSym])] deriving Show

myPhone :: Phone
myPhone = Phone $ zip rs as

-- 2.

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: Phone -> Char -> [(ReprSym, Presses)]
reverseTaps (Phone ((p, q) : xs)) c
  | isUpper c = (('*', 1) : reverseTaps (Phone ((p, q) : xs)) (toLower c))
  | eIRes /= Nothing = [(p, fromJust (eIRes)+1)] -- dangerous, but verified that it isn't Nothing
  | otherwise = reverseTaps (Phone xs) c
  where eIRes = elemIndex c q

cellPhonesDead :: Phone -> String -> [(ReprSym, Presses)]
cellPhonesDead p s = concat $ map (reverseTaps p) s

convoResult = map (cellPhonesDead myPhone) convo

-- 3.

fingerTaps :: [(ReprSym, Presses)] -> Presses
fingerTaps = foldr (\(_, y) b -> b + y) 0

-- 4.

mostPopularLetter :: String -> (Char, Int)
mostPopularLetter s = let (x, y) = (head (head l), length (head l))
                          l = sortBy (flip $ comparing length) $ group $ sort s
                      in (x, y)

costPopular :: Phone -> String -> Presses
costPopular p s = let (px, py) = mostPopularLetter s
                  in (py * (fingerTaps $ reverseTaps p px))
                     
-- 5. 

coolestLtr :: [String] -> Char
coolestLtr ps =
  fst
  (head
   (sortBy
    (flip (\(_, x) (_, y) -> compare x y))
    (map mostPopularLetter ps)))

coolestWord :: [String] -> String
coolestWord ps =
  head $ head $ sortBy (flip $ comparing length) $ group $ sort $ concat $ map words ps
  
-- I know I should abstract this, but gdamn am I tired    
