module Lib
  ( parseTest
  , anyChar
  , satisfy
  , char
  , digit
  , letter
  ) where

import           Control.Exception
import           Control.Monad.State
import           Data.Char

parseTest :: Show a => State String a -> String -> IO ()
parseTest f str =
  catch (print $ evalState f str) (\(SomeException e) -> putStr $ show e)

anyChar :: State String Char
anyChar = state (\(x:xs) -> (x, xs))

satisfy :: (Char -> Bool) -> State String Char
satisfy f =
  state
    (\(x:xs) ->
       case f x of
         True -> (x, xs))

char :: Char -> State String Char
char c = satisfy (== c)

digit, letter :: State String Char
digit = satisfy isDigit

letter = satisfy isLetter
