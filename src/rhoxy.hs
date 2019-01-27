-- Simple monadic rho-calc interpreter
module Main where

-- For getting commandline arguments
import System.Environment

import RhoParser

-- For executing
--import...


main :: IO ()
main = do
  args <- getArgs
  prog <- readFile $ head args
  print $ parseRhoc prog
