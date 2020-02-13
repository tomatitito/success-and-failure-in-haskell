module Main where

import Data.Char

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case length password > 20 || length password < 10 of
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

validatePassword :: String -> Maybe String
validatePassword password =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

reverseLine :: IO ()
reverseLine = do
  word <- getLine
  print $ reverse word

main :: IO ()
main = do
  putStr "Enter Password\n"
  password <- getLine
  print (validatePassword password)
