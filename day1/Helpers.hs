module Helpers (calculateDifferences, calculateScore) where

import Data.List (sort)

calculateDifferences :: [Int] -> [Int] -> Int
calculateDifferences left right =
  sum $ zipWith (\x y -> abs (x - y)) (sort left) (sort right)

calculateScore :: [Int] -> [Int] -> Int
calculateScore left right =
  let similarities = map (\x -> length $ filter (== x) right) left
   in sum $ zipWith (*) left similarities
