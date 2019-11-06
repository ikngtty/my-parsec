module Main where

import           Lib

anyChar2 :: String -> (String, String)
anyChar2 xs0 =
  let (x1, xs1) = anyChar xs0
      (x2, xs2) = anyChar xs1
   in ([x1, x2], xs2)

anyChar3 :: String -> (String, String)
anyChar3 xs0 =
  let (x12, xs12) = anyChar2 xs0
      (x3, xs3) = anyChar xs12
   in (x12 ++ [x3], xs3)

ldd :: String -> (String, String)
ldd xs0 =
  let (x1, xs1) = letter xs0
      (x2, xs2) = digit xs1
      (x3, xs3) = digit xs2
   in ([x1, x2, x3], xs3)

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
