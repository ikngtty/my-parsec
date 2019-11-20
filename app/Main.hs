module Main where

import           Control.Monad.State
import           Text.Parsec

-- anyChar2 :: StateT String (Either String) String
anyChar2 = sequence [anyChar, anyChar]

-- anyChar3 :: StateT String (Either String) String
anyChar3 = do
  x12 <- anyChar2
  x3 <- anyChar
  return $ x12 ++ [x3]

-- ldd :: StateT String (Either String) String
ldd = sequence [letter, digit, digit]

-- letterOrDigit :: StateT String (Either String) Char
letterOrDigit = letter <|> digit

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
  parseTest letterOrDigit "h"
  parseTest letterOrDigit "2"
  parseTest letterOrDigit ""
  parseTest (many letter) "abc123"
  parseTest (many letter) "123abc"
  parseTest (many letter) "abc"

checkErrorSensitivity :: IO ()
checkErrorSensitivity = do
  parseTest abOrAc "zzz"
  parseTest abOrAc "abz"
  parseTest abOrAc "acz"
  parseTest abOrAc "azz"
  parseTest abOrAc "a"
  parseTest abOrAc ""
  parseTest abOrCd "cde"
  where
    abOrAc = sequence [char 'a', char 'b'] <|> sequence [char 'a', char 'c']
    abOrCd = sequence [char 'a', char 'b'] <|> sequence [char 'c', char 'd']
