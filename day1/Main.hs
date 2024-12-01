module Main where

import Data.List (sort)

main :: IO ()
main = do
  putStrLn "Day 1, Part 1"

  contents <- readFile "./day1/input.txt"

  let fileLines = map words (lines contents)
      firstElements = sort $ map (read . head) fileLines :: [Int]
      secondElements = sort $ map (read . last) fileLines :: [Int]
      differences = zipWith (\x y -> abs (x - y)) firstElements secondElements
      summedDifferences = sum differences

  putStrLn $ "Part 1 answer (1970720): " ++ show summedDifferences

  putStrLn "\nDay 1, Part 2"

  let similarities = map (\x -> length $ filter (== x) secondElements) firstElements
      totalScore = sum $ zipWith (*) firstElements similarities

  putStrLn $ "Part 2 answer (17191599): " ++ show totalScore
