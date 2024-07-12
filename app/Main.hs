module Main where

import Control.Monad (liftM)
import Errors
import Eval
import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaluated <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ Errors.extractValue $ Errors.trapError evaluated
