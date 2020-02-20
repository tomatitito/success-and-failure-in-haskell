module Main where

import Data.Char

newtype Password = Password String deriving Show
newtype Username = Username String deriving Show
newtype Error = Error String deriving Show

data User = User Username Password deriving Show

makeUser :: Username -> Password -> Either Error User
makeUser username password =
  User <$> validateUsername username
       <*> validatePassword password

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case length password > 20 || length password < 10 of
    True -> Left (Error "Your password must be between 10 and 20 characters long")
    False -> Right (Password password)
    
checkUsernameLength :: String -> Either Error Username
checkUsernameLength username =
  case length username > 15 of
    True -> Left (Error "Username cannot be longer than 15 characters")
    False -> Right (Username username)

checkLength :: Int -> String -> Either Error String
checkLength len xs =
  case (length xs) > len of
    True -> Left (Error "Too many characters given")
    False -> Right xs

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

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
--    (bindMaybe (bindMaybe (cleanWhitespace password) requireAlphaNum) checkPasswordLength)
  do
    password' <- cleanWhitespace password
    password'' <- requireAlphaNum password'
    checkPasswordLength password''
--  cleanWhitespace password
--    >>= requireAlphaNum
--    >>= checkPasswordLength

makeUserTmpPassword :: Username -> Either Error User
makeUserTmpPassword username =
  User <$> validateUsername username
       <*> (validatePassword $ Password "temporaryPassword")
--       This also works
--       <*> pure (Password "temporayPassword")

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username
    >>= requireAlphaNum
    >>= checkUsernameLength

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
main = 
-- Original Version:
  do
    putStr "Enter username\n"
    username <- Username <$> getLine
    putStr "Enter password\n"
    password <-Password <$> getLine
    print (makeUser username password)
-- Alternative Version:
--  putStr "Enter Password\n"
--    >> Password <$> getLine
--    >>= print <$> validatePassword


--printTestResult :: Either Error () -> IO ()
--printTestResult r =
--  case r of
--    Left (Error err) -> putStrLn err
--    Right () -> putStrLn "All tests passed."
--
--eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
--eq n actual expected =
--  case (actual == expected) of
--    True -> Right ()
--    False -> Left (unlines ["Test " ++ show n , " Expected: " ++ show expected, " But got: " ++ show actual])
--
--test :: IO ()
--test = printTestResult $
--  do
--    eq 1 (checkPasswordLength "") (checkPasswordLength "")--(Left (Error "Your password must be between 10 and 20 characters long"))
----    eq 2 (checkPasswordLength "julielovesbooks") (Right $ Password "julielovesbooks")
----    eq 3 (validatePassword "1234567890") (Left $ Error "Your password must be between 10 and 20 characters long")
