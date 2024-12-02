module Main where

import Helpers (isSorted, permutationsByRemovingOne)

main :: IO ()
main = do
  putStrLn "Day 2, Part 1"

  contents <- readFile "./day2/input.txt"

  let fileLines = map (map read . words) (lines contents) :: [[Int]]
      isValidDiff x = x >= 1 && x <= 3
      differences arr = map (\line -> zipWith (\x y -> abs (x - y)) line (tail line)) $ filter isSorted arr
      validCount = length $ filter (all isValidDiff) $ differences fileLines

  putStrLn $ "Part 1 answer (379): " ++ show validCount

  putStrLn "\nDay 2, Part 2"

  let toleratedCount = length $ filter (any (all isValidDiff)) $ map (differences . permutationsByRemovingOne) fileLines

  putStrLn $ "Part 2 answer (430): " ++ show toleratedCount
