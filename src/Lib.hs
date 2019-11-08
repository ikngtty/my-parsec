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

parseTest :: Show a => State String (Either String a) -> String -> IO ()
parseTest f str =
  case evalState f str of
    Right r -> print r
    Left e  -> putStrLn $ "[parser ERROR] " ++ e

anyChar :: State String (Either String Char)
anyChar = state anyChar
  where
    anyChar (x:xs) = (Right x, xs)
    anyChar []     = (Left "no char", [])

satisfy :: (Char -> Bool) -> State String (Either String Char)
satisfy f = state satisfy
  where
    satisfy (x:xs)
      | f x = (Right x, xs)
      | otherwise = (Left $ ": " ++ show x, xs)
    satisfy [] = (Left ": no char", [])

char :: Char -> State String (Either String Char)
char c = (<|>) <$> satisfy (== c) <*> base
  where
    base :: State String (Either String Char)
    base = pure $ Left $ "not " ++ show c

digit, letter :: State String (Either String Char)
digit = (<|>) <$> satisfy isDigit <*> base
  where
    base :: State String (Either String Char)
    base = pure $ Left "not a digit"

letter = (<|>) <$> satisfy isLetter <*> base
  where
    base :: State String (Either String Char)
    base = pure $ Left "not a letter"
