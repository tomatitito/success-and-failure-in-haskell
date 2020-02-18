module Main where

import Data.Char

newtype Password = Password String deriving Show
newtype Username = Username String deriving Show
newtype Error = Error String deriving Show

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case length password > 20 || length password < 10 of
    True -> Left (Error "Your password must be between 10 and 20 characters long")
    False -> Right (Password password)
    
checkUsernameLength :: String -> Either Error Username
checkPasswordLength username =
  case length username > 15 of
    True -> Left (Error "Username cannot be longer than 15 characters")
    False -> Right (Username username)

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  case all isAlphaNum xs of
    False -> Left (Error "Only alphanumeric characters are allowed")
    True -> Right xs

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Empty string is not allowed")
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

printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines
      ["Test " ++ show n , " Expected: " ++ show expected, " But got: " ++ show actual])

test :: IO ()
test = printTestResult $
  do
    eq 1 (checkPasswordLength "") (Left "Your password must be between 10 and 20 characters long")
    eq 2 (checkPasswordLength "julielovesbooks") (Right "julielovesbooks")
    eq 3 (validatePassword "1234567890") (Left "Your password must be between 10 and 20 characters long")
