module Parser where

import Control.Monad (liftM)
import Errors as Errors
import Text.ParserCombinators.Parsec hiding (spaces)
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

readExpr :: String -> Errors.ThrowsError LispVal
readExpr input = case parse parseExpr "parser.hs" input of
  Left err -> Errors.throwError $ Errors.Parser err
  Right val -> return val
