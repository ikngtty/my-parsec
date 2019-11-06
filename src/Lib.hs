module Lib
  ( parseTest
  , anyChar
  , satisfy
  , char
  , digit
  , letter
  ) where

import           Control.Exception
import           Data.Char

parseTest :: Show a => (String -> (a, b)) -> String -> IO ()
parseTest f str =
  catch (print $ fst $ f str) (\(SomeException e) -> putStr $ show e)

anyChar :: String -> (Char, String)
anyChar (x:xs) = (x, xs)

satisfy :: (Char -> Bool) -> String -> (Char, String)
satisfy f (x:xs)
  | f x = (x, xs)

char :: Char -> String -> (Char, String)
char c = satisfy (== c)

digit, letter :: String -> (Char, String)
digit = satisfy isDigit

letter = satisfy isLetter
