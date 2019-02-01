-- Simple monadic rho-calc interpreter
module Main where

-- For getting commandline arguments
import System.Environment

import RhoParser
import RhoTypes
import Evaluator


main :: IO ()
main = do
  args <- getArgs
  prog <- readFile $ head args
  let parseResult = parseRhoc prog
  case parseResult of
    Left e -> print e
    Right (Par pool) -> do
      putStrLn "Parsed AST:"
      print pool
      print $ autoReduce pool
