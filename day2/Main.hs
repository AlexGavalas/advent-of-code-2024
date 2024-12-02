module Main where

import Data.List (sort)
import Helpers (permutationsByRemovingOne)

main :: IO ()
main = do
  putStrLn "Day 2, Part 1"

  contents <- readFile "./day2/input.txt"

  let fileLines = map words (lines contents)
      isValidDiff x = x >= 1 && x <= 3
      linesRead = map (map read) fileLines :: [[Int]]
      isAscending line = all (uncurry (>=)) (zip line (tail line))
      isDescending line = all (uncurry (<=)) (zip line (tail line))
      isSorted line = isAscending line || isDescending line
      differences arr = map (\line -> zipWith (\x y -> abs (x - y)) line (tail line)) $ filter isSorted arr
      validCount = length $ filter (all isValidDiff) $ differences linesRead

  putStrLn $ "Part 1 answer (379): " ++ show validCount

  putStrLn "\nDay 2, Part 2"

  let toleratedCount = length $ filter (any (all isValidDiff)) $ map (differences . permutationsByRemovingOne) linesRead

  putStrLn $ "Part 2 answer (430): " ++ show toleratedCount
