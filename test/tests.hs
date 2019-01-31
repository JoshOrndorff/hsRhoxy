-- https://stackoverflow.com/a/20331512/4184410
-- http://hackage.haskell.org/package/HUnit

import Test.HUnit
import RhoParser
import RhoTypes

-- TODO I can't get this file to give me results or failures with cabal test
-- If I make it executible, it works as expected.
miscTests = TestList [
  TestCase (assertEqual "parsing Nil" (Right (Par [Nil])) (parseRhoc "Nil")),
  TestCase (assertEqual "parsing Send" (Right (Par [Send Nil Nil])) (parseRhoc "@Nil!(Nil)"))
  ]


commentTests = TestList [
  TestCase (assertEqual "c1" (Right (Par [Nil])) (parseRhoc "Nil//c")),
  TestCase (assertEqual "c2" (Right (Par [Nil])) (parseRhoc "Nil// c")),
  TestCase (assertEqual "c3" (Right (Par [Nil])) (parseRhoc "Nil //comment")),
  TestCase (assertEqual "c4" (Right (Par [Nil])) (parseRhoc "Nil // comment")),
  TestCase (assertEqual "c5" (Right (Par [Nil])) (parseRhoc "Nil/* comment*/")),
  TestCase (assertEqual "c6" (Right (Par [Nil])) (parseRhoc "Nil /* comment */")),
  TestCase (assertEqual "c7" (Right (Par [Nil])) (parseRhoc "/* c */ Nil /* c */")),
  TestCase (assertEqual "c8" (Right (Par [Nil])) (parseRhoc "/**/Nil")),
  TestCase (assertEqual "c9" (Right (Par [Nil])) (parseRhoc "Nil/**/")),
  TestCase (assertEqual "c10" (Right (Par [Nil])) (parseRhoc "Nil /* c */ //c")),
  TestCase (assertEqual "c11" (Right (Par [Nil])) (parseRhoc "Nil//")),
  TestCase (assertEqual "c12" (Right (Par [Nil])) (parseRhoc "/* c */ Nil //c")),
  TestCase (assertEqual "c13" (Right (Par [Nil])) (parseRhoc "/**//**/Nil"))
  ]


main = runTestTT commentTests
