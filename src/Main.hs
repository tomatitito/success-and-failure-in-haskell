module Main where

import Data.Char

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case length password > 20 || length password < 10 of
    True -> Left "Your password must be between 10 and 20 characters long"
    False -> Right password

requireAlphaNum :: String -> Either String String
requireAlphaNum password =
  case all isAlphaNum password of
    False -> Left "Your password can only contain alphanumeric characters"
    True -> Right password

-- TODO: Refactor using Either
cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Password must not be empty"
cleanWhitespace (x:xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Right (x:xs)

validatePassword :: String -> Either String String
validatePassword password =
--    (bindMaybe (bindMaybe (cleanWhitespace password) requireAlphaNum) checkPasswordLength)
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

reverseLine :: IO ()
reverseLine = do
  word <- getLine
  print $ reverse word

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
--bindMaybe = (>>=)
bindMaybe (Just a) f = f a
bindMaybe Nothing f = Nothing

data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue x f =
  case x of
    Str x' -> Str x'
    Val x' -> f x'

main :: IO ()
main = do
  putStr "Enter Password\n"
  password <- getLine
  print (validatePassword password)
