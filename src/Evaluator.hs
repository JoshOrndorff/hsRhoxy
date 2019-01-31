module Evaluator where

import RhoTypes

sub :: Proc -> String -> Proc -> Proc
sub peg binder target =
  case target of
    Nil -> Nil
    Send c p -> Send (sub peg binder c) (sub peg binder p)
    Recv c p1 p2 -> -- TODO Is this where the `=` operator is important? or shaddowing?
      Recv (sub peg binder c) (sub peg binder p1) (sub peg binder p2)
    FreeName f -> if binder == f then peg else FreeName f
    Par ps -> Par $ map (sub peg binder) $ ps
