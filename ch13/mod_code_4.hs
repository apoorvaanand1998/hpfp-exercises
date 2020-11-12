import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Show, Eq)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                "Name was: " ++ show name ++
                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Please input Person's name: "
  name <- getLine
  putStr "Please input Person's age: "
  a <- getLine
  let age = read a :: Integer
  case (mkPerson name age) of
    Right x -> putStrLn $ "Yay! Successfully got a person: " ++
             show x
    Left x -> putStrLn $ "Oops! An error occured: " ++ show x
