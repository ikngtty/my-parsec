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

instance Semigroup ErrorInfo where
  ErrorInfo a <> ErrorInfo b = ErrorInfo $ a <> b

instance Monoid ErrorInfo where
  mempty = ErrorInfo (mempty :: String)
  mappend = (<>)

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

parseTest :: Show a => StateT String (Either ErrorInfo) a -> String -> IO ()
parseTest state text =
  case evalStateT state text of
    Right r -> print r
    Left (ErrorInfo e) ->
      putStrLn $ "[parser ERROR] " ++ show text ++ " -> " ++ e

anyChar :: StateT String (Either ErrorInfo) Char
anyChar = StateT anyChar
  where
    anyChar (x:xs) = Right (x, xs)
    anyChar []     = Left $ ErrorInfo "too short"

satisfy :: (Char -> Bool) -> StateT String (Either ErrorInfo) Char
satisfy f = StateT satisfy
  where
    satisfy (x:xs)
      | f x = Right (x, xs)
      | otherwise = Left $ ErrorInfo (": " ++ show x)
    satisfy [] = Left $ ErrorInfo ": no char"

char :: Char -> StateT String (Either ErrorInfo) Char
char c = satisfy (== c) <|> (lift . Left) (ErrorInfo $ "not " ++ show c)

digit :: StateT String (Either ErrorInfo) Char
digit = satisfy isDigit <|> (lift . Left) (ErrorInfo "not a digit")

letter :: StateT String (Either ErrorInfo) Char
letter = satisfy isLetter <|> (lift . Left) (ErrorInfo "not a letter")

many ::
     StateT String (Either ErrorInfo) Char
  -> StateT String (Either ErrorInfo) String
many parser = StateT $ parse ""
  where
    parse :: String -> String -> Either ErrorInfo (String, String)
    parse acm s =
      case runStateT parser s of
        Left _        -> Right (acm, s)
        Right (c, cs) -> parse (acm ++ [c]) cs
