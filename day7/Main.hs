{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (replicateM)
import Data.Foldable (find, foldl')
import Lib (splitOn)

getAllOperatorCombinations :: [String] -> [[String]]
getAllOperatorCombinations l = replicateM (length l - 1) ["+", "*"]

extractValue :: Either String String -> String
extractValue = \case
  Left n -> n
  Right o -> o

interleave :: [a] -> [b] -> [Either a b]
interleave [] ys = map Right ys
interleave xs [] = map Left xs
interleave (x : xs) (y : ys) = Left x : Right y : interleave xs ys

getInterleavedList :: [String] -> [[String]]
getInterleavedList l = map (map extractValue . interleave l) $ getAllOperatorCombinations l

calculate :: Int -> [String] -> Int
calculate target = fst . foldl' step (0, "+")
  where
    step (runningTotal, op) item =
      case item of
        "+" -> (runningTotal, "+")
        "*" -> (runningTotal, "*")
        n ->
          let num = read n :: Int
           in if op == "+"
                then (runningTotal + num, op)
                else (runningTotal * num, op)

performCalculations :: String -> Int -> [Int]
performCalculations l target =
  let strList = filter (not . null) (splitOn ' ' l)
      interleavedList = getInterleavedList strList
   in map (calculate target) interleavedList

main :: IO ()
main = do
  putStrLn "Day 7, Part 1"

  contents <- readFile "./day7/input.txt"

  let operations = lines contents
      formattedInput = map (splitOn ':') operations
      results =
        sum $
          map ((\v -> read v :: Int) . head) $
            filter
              ( \[targetTotal, l] ->
                  let n = read targetTotal :: Int
                      match = find (== n) $ performCalculations l n
                   in not (null match)
              )
              formattedInput

  putStrLn $ "Part 1 answer (267566105056): " ++ show results

-- putStrLn "\nDay 7, Part 2"

-- putStrLn $ "Part 2 answer (): "
