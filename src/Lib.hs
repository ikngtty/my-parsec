module Lib
  ( parseTest
  , anyChar
  , satisfy
  , char
  , digit
  , letter
  ) where

import           Control.Monad.State
import           Data.Char

parseTest :: Show a => State String (Either String a) -> String -> IO ()
parseTest f str =
  case evalState f str of
    Right r -> print r
    Left e  -> putStrLn $ "[parser ERROR] " ++ e

anyChar :: State String (Either String Char)
anyChar = state anyChar
  where
    anyChar (x:xs) = (Right x, xs)
    anyChar []     = (Left "too short", [])

satisfy ::
     (Char -> String) -> (Char -> Bool) -> State String (Either String Char)
satisfy err f = state satisfy
  where
    satisfy (x:xs)
      | f x = (Right x, xs)
      | otherwise = (Left $ err x, xs)
    satisfy [] = (Left "too short", [])

char :: Char -> State String (Either String Char)
char c = satisfy (\x -> show x ++ " is not " ++ show c) (== c)

digit, letter :: State String (Either String Char)
digit = satisfy (\x -> show x ++ " is not a digit") isDigit

letter = satisfy (\x -> show x ++ " is not a letter") isLetter
