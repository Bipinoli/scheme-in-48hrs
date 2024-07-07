module Main where

import Control.Monad (liftM)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

instance Show LispVal where show = showVal

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  str <- many (noneOf "\"")
  _ <- char '"'
  return $ String str

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  headLst <- endBy parseExpr spaces
  tailItem <- char '.' >> spaces >> parseExpr
  return $ DottedList headLst tailItem

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      _ <- char '('
      lst <- try parseList <|> parseDottedList
      _ <- char ')'
      return lst

readExpr :: String -> String
readExpr input = case parse parseExpr "Main.hs" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

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

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
