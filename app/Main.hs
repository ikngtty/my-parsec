module Main where

import           Control.Monad.State

import           Text.Parsec
import qualified Text.Parsec.String  as PS

-- import           Lib                 as PS
anyChar2 :: PS.Parser String
anyChar2 = sequence [anyChar, anyChar]

anyChar3 :: PS.Parser String
anyChar3 = do
  x12 <- anyChar2
  x3 <- anyChar
  return $ x12 ++ [x3]

ldd :: PS.Parser String
ldd = sequence [letter, digit, digit]

letterOrDigit :: PS.Parser Char
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
  parseTest abOrAbc "abc"
  parseTest abcOrAb "abc"
  parseTest abOrAc "zzz"
  parseTest abOrAc "abz"
  parseTest abOrAc "acz"
  parseTest abOrAc "azz"
  parseTest abOrAc "a"
  parseTest abOrAc ""
  parseTest abOrCd "cde"
  where
    abOrAbc =
      sequence [char 'a', char 'b'] <|> sequence [char 'a', char 'b', char 'c']
    abcOrAb =
      sequence [char 'a', char 'b', char 'c'] <|> sequence [char 'a', char 'b']
    abOrAc = sequence [char 'a', char 'b'] <|> sequence [char 'a', char 'c']
    abOrCd = sequence [char 'a', char 'b'] <|> sequence [char 'c', char 'd']

checkTry :: IO ()
checkTry = do
  putStrLn "--- ab or ac ---"
  parseTest abOrAc "zzz"
  parseTest abOrAc "abz"
  parseTest abOrAc "acz"
  parseTest abOrAc "azz"
  putStrLn "--- try ab or ac ---"
  parseTest tryAbOrAc "zzz"
  parseTest tryAbOrAc "abz"
  parseTest tryAbOrAc "acz"
  parseTest tryAbOrAc "azz"
  putStrLn "--- ab or try ac ---"
  parseTest abOrTryAc "zzz"
  parseTest abOrTryAc "abz"
  parseTest abOrTryAc "acz"
  parseTest abOrTryAc "azz"
  putStrLn "--- try ab or try ac ---"
  parseTest tryAbOrTryAc "zzz"
  parseTest tryAbOrTryAc "abz"
  parseTest tryAbOrTryAc "acz"
  parseTest tryAbOrTryAc "azz"
  where
    ab = sequence [char 'a', char 'b']
    ac = sequence [char 'a', char 'c']
    abOrAc = ab <|> ac
    tryAbOrAc = try ab <|> ac
    abOrTryAc = ab <|> try ac
    tryAbOrTryAc = try ab <|> try ac
