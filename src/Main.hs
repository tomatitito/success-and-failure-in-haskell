checkPasswordLength password =
  case length password > 20 of
    True -> Nothing
    False -> Just passwordsword > 20 of
      True -> Nothing
      False -> Just password

main :: IO ()
main = do
  putStr "Enter Password\n"
  password <- getLine
  print (checkPasswordLength password)
