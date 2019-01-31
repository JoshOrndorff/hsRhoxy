-- Simple monadic rho-calc interpreter
module Main where

-- For getting commandline arguments
import System.Environment

import RhoParser
import RhoTypes

-- For executing
--import...

sub :: Proc -> String -> Proc -> Proc
sub peg binder target =
  case target of
    Nil -> Nil
    Send c p -> Send (sub peg binder c) (sub peg binder p)
    Recv c p1 p2 -> -- TODO Is this where the `=` operator is important?
      Recv (sub peg binder c) (sub peg binder p1) (sub peg binder p2)
      -- I think it's better to do exactly this. Then there is no `=` operator
      -- If you want to shadow, do it explicitly with new
    FreeName f -> if binder == f then peg else FreeName f

main :: IO ()
main = do
  args <- getArgs
  prog <- readFile $ head args
  let parseResult = parseRhoc prog
  case parseResult of
    Left e -> print e
    Right ast -> do
      putStrLn "Parsed AST:"
      print ast
