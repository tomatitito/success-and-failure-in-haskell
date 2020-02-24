{-# LANGUAGE GeneralisedNewtypeDeriving #-}


module Main where


import Data.Char
import Data.Validation

newtype Password = Password String deriving (Show, Eq)
newtype Username = Username String deriving (Show, Eq)
newtype Error = Error [String] deriving (Show, Eq, Semigroup)

data User = User Username Password deriving Show

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> validateUsername username
       <*> validatePassword password

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  case length password > 20 || length password < 10 of
    True -> Failure (Error ["Your password must be between 10 and 20 characters long"])
    False -> Success (Password password)
    
checkUsernameLength :: String -> Validation Error Username
checkUsernameLength username =
  case length username > 15 of
    True -> Failure (Error ["Username cannot be longer than 15 characters"])
    False -> Success (Username username)

checkLength :: Int -> String -> Validation Error String
checkLength len xs =
  case (length xs) > len of
    True -> Failure (Error ["Too many characters given"])
    False -> Success xs

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case all isAlphaNum xs of
    False -> Failure (Error ["Only alphanumeric characters are allowed"])
    True -> Success xs

cleanWhitespace :: String -> Validation Error String
cleanWhitespace "" = Failure (Error ["Empty string is not allowed"])
cleanWhitespace (x:xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Success (x:xs)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
--  Original Version:
--  cleanWhitespace password
--    >>= requireAlphaNum
--    >>= checkPasswordLength
--  Alternative Version using bindMaybe: 
--    (bindMaybe (bindMaybe (cleanWhitespace password) requireAlphaNum) checkPasswordLength)
--  Alternative Version using do:
--  do
--    password' <- cleanWhitespace password
--    password'' <- requireAlphaNum password'
--    checkPasswordLength password''
--  Alternative Version in appliativa style:
    case cleanWhitespace password of
      Failure err -> Failure err
      Success password2 -> 
        requireAlphaNum password2 *> checkPasswordLength password2

makeUserTmpPassword :: Username -> Validation Error User
makeUserTmpPassword username =
  User <$> validateUsername username
       <*> (validatePassword $ Password "temporaryPassword")
--       This also works
--       <*> pure (Password "temporayPassword")

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case cleanWhitespace username of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2
                      *> checkUsernameLength username2
--  cleanWhitespace username
--    >>= requireAlphaNum
--    >>= checkUsernameLength

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
--  Original Version:
--  do
--    putStr "Enter username\n"
--    username <- Username <$> getLine
--    putStr "Enter password\n"
--    password <-Password <$> getLine
--    print (makeUser username password)
-- Alternative Version:
  putStr "Enter Password\n"
    >> Password <$> getLine
    >>= print <$> validatePassword


printTestResult :: Validation Error () -> IO ()
printTestResult r =
  case r of
    Failure (Error [err]) -> putStrLn err
    Success () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Validation Error ()
eq n actual expected =
  case (actual == expected) of
    True -> Success ()
    False -> Failure (Error [(unlines ["Test " ++ show n , " Expected: " ++ show expected, " But got: " ++ show actual])])

test :: IO ()
test = printTestResult $ 
           eq 1 (checkPasswordLength "") (Failure (Error ["Your password must be between 10 and 20 characters long"])) -- does not work, why? (checkPasswordLength "")
        *> eq 2 (checkPasswordLength "julielovesbooks") (Success $ Password "julielovesbooks")
        *> eq 3 (validatePassword (Password "1234567890")) (Failure $ Error ["Your password must be between 10 and 20 characters long"])
