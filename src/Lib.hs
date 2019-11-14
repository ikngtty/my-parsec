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

(<|>) :: Monoid l => Either l r -> Either l r -> Either l r
Left a <|> Left b = Left $ b <> a
Left _ <|> Right b = Right b
Right a <|> Left _ = Right a

parseTest :: Show a => StateT String (Either String) a -> String -> IO ()
parseTest state text =
  case evalStateT state text of
    Right r -> print r
    Left e  -> putStrLn $ "[parser ERROR] " ++ show text ++ " -> " ++ e

anyChar :: StateT String (Either String) Char
anyChar = StateT anyChar
  where
    anyChar (x:xs) = Right (x, xs)
    anyChar []     = Left "too short"

satisfy :: (Char -> Bool) -> StateT String (Either String) Char
satisfy f = StateT satisfy
  where
    satisfy (x:xs)
      | f x = Right (x, xs)
      | otherwise = Left $ ": " ++ show x
    satisfy [] = Left ": no char"

char :: Char -> StateT String (Either String) Char
char c = mapStateT addErrorBase $ satisfy (== c)
  where
    addErrorBase :: Either String (Char, String) -> Either String (Char, String)
    addErrorBase result = result <|> Left ("not " ++ show c)

digit :: StateT String (Either String) Char
digit = mapStateT addErrorBase $ satisfy isDigit
  where
    addErrorBase :: Either String (Char, String) -> Either String (Char, String)
    addErrorBase result = result <|> Left "not a digit"

letter :: StateT String (Either String) Char
letter = mapStateT addErrorBase $ satisfy isLetter
  where
    addErrorBase :: Either String (Char, String) -> Either String (Char, String)
    addErrorBase result = result <|> Left "not a letter"
