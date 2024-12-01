import Helpers (calculateDifferences, calculateScore)
import Test.HUnit

-- Test data
sampleInput :: [(Int, Int)]
sampleInput = [(3, 4), (4, 3), (2, 5), (1, 3), (3, 9), (3, 3)]

-- Test for Part 1
testDifferences :: Test
testDifferences = TestCase $ do
  let (left, right) = unzip sampleInput
      result = calculateDifferences left right

  assertEqual "Calculates correct differences" 11 result

-- Test for Part 2
testScore :: Test
testScore = TestCase $ do
  let (left, right) = unzip sampleInput
      result = calculateScore left right

  assertEqual "Calculates correct score" 31 result

-- Test suite
tests :: Test
tests =
  TestList
    [ TestLabel "Test Differences" testDifferences,
      TestLabel "Test Score" testScore
    ]

main :: IO Counts
main = runTestTT tests
