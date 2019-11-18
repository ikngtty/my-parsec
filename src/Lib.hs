module Lib
  ( parseTest
  , anyChar
  , satisfy
  , char
  , digit
  , letter
  , many
  , (<|>)
  ) where

import           Control.Monad.State
import           Data.Char

(<|>) ::
     Monoid l
  => StateT s (Either l) r
  -> StateT s (Either l) r
  -> StateT s (Either l) r
StateT a <|> StateT b = StateT $ \s -> a s <|> b s
  where
    (<|>) :: Monoid l => Either l (r, s) -> Either l (r, s) -> Either l (r, s)
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
char c = satisfy (== c) <|> (lift . Left) ("not " ++ show c)

digit :: StateT String (Either String) Char
digit = satisfy isDigit <|> (lift . Left) "not a digit"

letter :: StateT String (Either String) Char
letter = satisfy isLetter <|> (lift . Left) "not a letter"

many ::
     StateT String (Either String) Char -> StateT String (Either String) String
many parser = StateT $ parse ""
  where
    parse :: String -> String -> Either String (String, String)
    parse acm s =
      case runStateT parser s of
        Left _        -> Right (acm, s)
        Right (c, cs) -> parse (acm ++ [c]) cs
