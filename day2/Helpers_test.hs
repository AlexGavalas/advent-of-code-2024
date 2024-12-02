import Helpers (permutationsByRemovingOne)
import Test.HUnit

-- Test data
sampleInput :: [Int]
sampleInput = [1, 2, 3, 4]

testPermutations :: Test
testPermutations = TestCase $ do
  let result = permutationsByRemovingOne sampleInput
      expected = [[2, 3, 4], [1, 3, 4], [1, 2, 4], [1, 2, 3]]

  assertEqual "Calculates correct permutations" expected result

-- Test suite
tests :: Test
tests =
  TestList
    [ TestLabel "Test permutations" testPermutations
    ]

main :: IO Counts
main = runTestTT tests
