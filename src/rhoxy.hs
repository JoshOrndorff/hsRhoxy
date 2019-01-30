-- Simple monadic rho-calc interpreter
module Main where

-- For getting commandline arguments
import System.Environment

import RhoParser

-- For executing
--import...

subN :: Proc -> String -> Chan -> Chan
subN peg name (Quote p) = Quote $ sub peg name p

sub :: Proc -> String -> Proc -> Proc
sub _   _    Nil = Nil
sub peg name (Unquote c) = Unquote $ subN peg name c
sub peg name (Send c p) =
  Send (subN peg name c) (sub peg name p)
sub peg name (Recv c p1 p2) = Recv (subN peg name c) (sub peg name p1) (sub peg name p2)
sub peg name (Hole n) =
  if name == n
  then peg
  else Hole n

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
