module Main where

import Eval
import Parser
import System.Environment
import Types

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
