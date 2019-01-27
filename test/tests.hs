-- https://stackoverflow.com/a/20331512/4184410
-- http://hackage.haskell.org/package/HUnit

import Test.HUnit
import RhoParser

-- TODO I can't get this file to give me results or failures with cabal test
-- If I make it executible, it works as expected.
tests = TestList [
  TestCase (assertEqual "parsing Nil" (Right (Par [Nil])) (parseRhoc "Nil")),
  TestCase (assertEqual "parsing Send" (Right (Par [Send (Quote Nil) Nil])) (parseRhoc "@Nil!(Nil)")),
  TestCase (assertEqual "for (foo 3)," (1,2) (1,3))
  ]

main = runTestTT tests
