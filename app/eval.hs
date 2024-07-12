module Eval where

import qualified Control.Monad.Error as Errors
import Errors as Errors
import qualified Errors as Errros
import Types

eval :: LispVal -> Errors.ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm

apply :: String -> [LispVal] -> Errors.ThrowsError LispVal
apply func args =
  maybe
    (Errors.throwError $ Errors.NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> Errors.ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Errors.ThrowsError LispVal
numericBinop op [] = Errors.throwError $ Errors.NumArgs 2 []
numericBinop op singleVal@[_] = Errors.throwError $ Errros.NumArgs 2 singleVal
numericBinop op args = mapM unpackNumber args >>= return . Number . foldl1 op

unpackNumber :: LispVal -> Errors.ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then Errors.throwError $ Errors.TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNumber = Errors.throwError $ Errors.TypeMismatch "number" notNumber
