{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

newtype ErrorInfo =
  ErrorInfo String
  deriving (Semigroup, Monoid)

(<|>) ::
     (Monoid l, Eq s)
  => StateT s (Either (l, s)) r
  -> StateT s (Either (l, s)) r
  -> StateT s (Either (l, s)) r
StateT a <|> StateT b =
  StateT $ \s ->
    case (a s, b s) of
      (Right a, _) -> Right a
      (Left (ea, sa), Left (eb, _)) -> Left (eb <> ea, sa)
      (Left (ea, sa), Right b)
        | sa == s -> Right b
        | otherwise -> Left (ea, sa)

parseTest ::
     Show a => StateT String (Either (ErrorInfo, String)) a -> String -> IO ()
parseTest state text =
  case evalStateT state text of
    Right r -> print r
    Left (ErrorInfo e, _) ->
      putStrLn $ "[parser ERROR] " ++ show text ++ " -> " ++ e

anyChar :: StateT String (Either (ErrorInfo, String)) Char
anyChar = StateT anyChar
  where
    anyChar (x:xs) = Right (x, xs)
    anyChar []     = Left (ErrorInfo "too short", "")

satisfy :: (Char -> Bool) -> StateT String (Either (ErrorInfo, String)) Char
satisfy f = StateT satisfy
  where
    satisfy (x:xs)
      | f x = Right (x, xs)
      | otherwise = Left (ErrorInfo (": " ++ show x), x : xs)
    satisfy [] = Left (ErrorInfo ": no char", "")

char :: Char -> StateT String (Either (ErrorInfo, String)) Char
char c =
  satisfy (== c) <|> (lift . Left) (ErrorInfo ("not " ++ show c), undefined)

digit :: StateT String (Either (ErrorInfo, String)) Char
digit = satisfy isDigit <|> (lift . Left) (ErrorInfo "not a digit", undefined)

letter :: StateT String (Either (ErrorInfo, String)) Char
letter =
  satisfy isLetter <|> (lift . Left) (ErrorInfo "not a letter", undefined)

many ::
     StateT String (Either (ErrorInfo, String)) Char
  -> StateT String (Either (ErrorInfo, String)) String
many parser = StateT $ parse ""
  where
    parse :: String -> String -> Either (ErrorInfo, String) (String, String)
    parse acm s =
      case runStateT parser s of
        Left _        -> Right (acm, s)
        Right (c, cs) -> parse (acm ++ [c]) cs
