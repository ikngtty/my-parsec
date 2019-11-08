module Main where

import           Control.Monad.State
import           Lib

anyChar2 :: State String String
anyChar2 = sequence [anyChar, anyChar]

anyChar3 :: State String String
anyChar3 = do
  x12 <- anyChar2
  x3 <- anyChar
  return $ x12 ++ [x3]

ldd :: State String String
ldd = sequence [letter, digit, digit]

main :: IO ()
main = do
  parseTest anyChar "abcde"
  parseTest anyChar2 "abcde"
  parseTest anyChar3 "abcde"
  parseTest anyChar3 "ab"
  parseTest (char 'a') "abc"
  parseTest (char 'a') "bcd"
  parseTest digit "abc"
  parseTest digit "123"
  parseTest letter "abc"
  parseTest letter "123"
  parseTest ldd "a12abc"
  parseTest ldd "012abc"
  parseTest ldd "ab2abc"
  parseTest ldd "a1nabc"
