module Main where

import Helpers (calculateDifferences, calculateScore)

main :: IO ()
main = do
  putStrLn "Day 1, Part 1"

  contents <- readFile "./day1/input.txt"

  let fileLines = map words (lines contents)
      (left, right) = unzip $ map (\x -> (read (head x), read (last x))) fileLines :: ([Int], [Int])

  putStrLn $ "Part 1 answer (1970720): " ++ show (calculateDifferences left right)

  putStrLn "\nDay 1, Part 2"

  putStrLn $ "Part 2 answer (17191599): " ++ show (calculateScore left right)
