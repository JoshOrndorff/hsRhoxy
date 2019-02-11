-- https://stackoverflow.com/a/20331512/4184410
-- http://hackage.haskell.org/package/HUnit


module Main
( main
) where

import Test.HUnit
import qualified System.Exit as Exit -- needed this for exitcode-stdio-1.0


import RhoParser
import RhoTypes
import Evaluator


-- TODO Move parser and other tests into several files.


miscTests = TestList [
  TestCase (assertEqual "parsing Nil" (Right (Par [Nil ()])) (parseRhoc "Nil")),
  TestCase (assertEqual "parsing Send" (Right (Par [Send (Nil ()) (Nil ())])) (parseRhoc "@Nil!(Nil)"))
  ]


commentTests = TestList [
  TestCase (assertEqual "c1" (Right (Par [Nil ()])) (parseRhoc "Nil//c")),
  TestCase (assertEqual "c2" (Right (Par [Nil ()])) (parseRhoc "Nil// c")),
  TestCase (assertEqual "c3" (Right (Par [Nil ()])) (parseRhoc "Nil //comment")),
  TestCase (assertEqual "c4" (Right (Par [Nil ()])) (parseRhoc "Nil // comment")),
  TestCase (assertEqual "c5" (Right (Par [Nil ()])) (parseRhoc "Nil/* comment*/")),
  TestCase (assertEqual "c6" (Right (Par [Nil ()])) (parseRhoc "Nil /* comment */")),
  TestCase (assertEqual "c7" (Right (Par [Nil ()])) (parseRhoc "/* c */ Nil /* c */")),
  TestCase (assertEqual "c8" (Right (Par [Nil ()])) (parseRhoc "/**/Nil")),
  TestCase (assertEqual "c9" (Right (Par [Nil ()])) (parseRhoc "Nil/**/")),
  TestCase (assertEqual "c10" (Right (Par [Nil ()])) (parseRhoc "Nil /* c */ //c")),
  TestCase (assertEqual "c11" (Right (Par [Nil ()])) (parseRhoc "Nil//")),
  TestCase (assertEqual "c12" (Right (Par [Nil ()])) (parseRhoc "/* c */ Nil //c")),
  TestCase (assertEqual "c13" (Right (Par [Nil ()])) (parseRhoc "/**//**/Nil"))
  ]



subTests = TestList [
  -- Peg Binder Target
  TestCase (assertEqual "s1" (Nil ()) (sub (Nil ()) "P" (FreeName "P"))),
  TestCase (assertEqual "s2" (FreeName "Q") (sub (Nil ()) "P" (FreeName "Q"))),
  TestCase (assertEqual "s3" (Par [(Nil ()), FreeName "Q"]) (sub (Nil ()) "P" (Par [FreeName "P", FreeName "Q"])))
  ]

-- This is the main thing that gets exported with detailed-0.9
-- But I never got it working because of fucked up imports
-- Advice from https://stackoverflow.com/a/11874602/4184410
--import qualified Distribution.TestSuite as Cabal -- comes from Cabal >2.2 && < 3
--import qualified Distribution.TestSuite.HUnit as CabalHUnit -- couldn't find
--tests = map (\(x,y) -> CabalHUnit.test x y) [("comment tests", commentTests)]


-- This one works with exitcode-stdio-1.0
-- Main program taken from https://gist.github.com/23Skidoo/8019225
main :: IO ()
main = do
  count <- runTestTT commentTests
  if failures count > 0 then Exit.exitFailure else return ()
