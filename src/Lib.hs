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

parseTest :: Show a => State String (Maybe a) -> String -> IO ()
parseTest f str =
  case evalState f str of
    Just r  -> print r
    Nothing -> putStrLn "Error!"

anyChar :: State String (Maybe Char)
anyChar = state anyChar
  where
    anyChar (x:xs) = (Just x, xs)
    anyChar []     = (Nothing, [])

satisfy :: (Char -> Bool) -> State String (Maybe Char)
satisfy f = state satisfy
  where
    satisfy (x:xs)
      | f x = (Just x, xs)
      | otherwise = (Nothing, xs)
    satisfy [] = (Nothing, [])

char :: Char -> State String (Maybe Char)
char c = satisfy (== c)

digit, letter :: State String (Maybe Char)
digit = satisfy isDigit

letter = satisfy isLetter
