module Main where

import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case length password > 20 of
    True -> Nothing
    False -> Just password

requireAlphaNum :: String -> Maybe String
requireAlphaNum password =
  case all isAlphaNum password of
    False -> Nothing
    True -> Just password

main :: IO ()
main = do
  putStr "Enter Password\n"
  password <- getLine
  print (requireAlphaNum password)
