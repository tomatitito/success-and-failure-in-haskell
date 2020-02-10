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

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x:xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Just (x:xs)
    
main :: IO ()
main = do
  putStr "Enter Password\n"
  password <- getLine
  print (cleanWhitespace password)
