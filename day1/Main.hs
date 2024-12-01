module Main where

import Data.List (sort)

main :: IO ()
main = do
  putStrLn "Day 1, Part 1"

  contents <- readFile "./day1/input.txt"

  let fileLines = map words (lines contents)
      (leftColumn, rightColumn) = unzip $ map (\x -> (read (head x), read (last x))) fileLines :: ([Int], [Int])
      differences = zipWith (\x y -> abs (x - y)) (sort leftColumn) (sort rightColumn)
      summedDifferences = sum differences

  putStrLn $ "Part 1 answer (1970720): " ++ show summedDifferences

  putStrLn "\nDay 1, Part 2"

  let similarities = map (\x -> length $ filter (== x) rightColumn) leftColumn
      totalScore = sum $ zipWith (*) leftColumn similarities

  putStrLn $ "Part 2 answer (17191599): " ++ show totalScore
