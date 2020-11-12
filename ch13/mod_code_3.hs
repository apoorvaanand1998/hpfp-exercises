import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (modLine line1 == reverse (modLine line1)) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
  where modLine = map toLower . filter isAlpha
