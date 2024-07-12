module Types where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom atomVal) = atomVal
showVal (Number numVal) = show numVal
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List lst) = "(" ++ unwordsList lst ++ ")"
showVal (DottedList lst tailItem) = "(" ++ unwordsList lst ++ " . " ++ showVal tailItem ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
