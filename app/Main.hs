module Main where

import           Control.Monad.State
import           Lib

anyChar2 :: State String (Either String String)
anyChar2 = do
  x1 <- anyChar
  x2 <- anyChar
  return $ sequence [x1, x2]

anyChar3 :: State String (Either String String)
anyChar3 = do
  x12 <- anyChar2
  x3 <- anyChar
  return $ (++) <$> x12 <*> fmap pure x3

ldd :: State String (Either String String)
ldd = do
  x1 <- letter
  x2 <- digit
  x3 <- digit
  return $ sequence [x1, x2, x3]

main :: IO ()
main = do
  parseTest anyChar "abcde"
  parseTest anyChar2 "abcde"
  parseTest anyChar3 "abcde"
  parseTest anyChar3 "ab"
  parseTest (char 'a') "abc"
  parseTest (char 'a') "bcd"
  parseTest (char 'a') ""
  parseTest digit "abc"
  parseTest digit "123"
  parseTest digit ""
  parseTest letter "abc"
  parseTest letter "123"
  parseTest letter ""
  parseTest ldd "a12abc"
  parseTest ldd "012abc"
  parseTest ldd "ab2abc"
  parseTest ldd "a1nabc"
