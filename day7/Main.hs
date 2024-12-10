{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (replicateM)
import Data.Foldable (find, foldl')
import Lib (splitOn)

operatorsPart1 :: [String]
operatorsPart1 = ["+", "*"]

operatorsPart2 :: [String]
operatorsPart2 = ["+", "*", "||"]

getAllOperatorCombinations :: [String] -> [String] -> [[String]]
getAllOperatorCombinations l = replicateM (length l - 1)

extractValue :: Either String String -> String
extractValue = \case
  Left n -> n
  Right o -> o

interleave :: [a] -> [b] -> [Either a b]
interleave [] ys = map Right ys
interleave xs [] = map Left xs
interleave (x : xs) (y : ys) = Left x : Right y : interleave xs ys

getInterleavedList :: [String] -> [String] -> [[String]]
getInterleavedList l ops = map (map extractValue . interleave l) $ getAllOperatorCombinations l ops

calculatePart1 :: Int -> [String] -> Int
calculatePart1 target = fst . foldl' step (0, "+")
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

performCalculationsPart1 :: String -> Int -> [Int]
performCalculationsPart1 l target =
  let strList = filter (not . null) (splitOn ' ' l)
      interleavedList = getInterleavedList strList operatorsPart1
   in map (calculatePart1 target) interleavedList

calculatePart2 :: Int -> [String] -> Int
calculatePart2 target = fst . foldl' step (0, "+")
  where
    step (runningTotal, op) item =
      case item of
        "+" -> (runningTotal, "+")
        "*" -> (runningTotal, "*")
        "||" -> (runningTotal, "||")
        n ->
          let num = read n :: Int
           in case op of
                "+" -> (runningTotal + num, op)
                "*" -> (runningTotal * num, op)
                "||" -> (read (show runningTotal ++ n), op)

performCalculationsPart2 :: String -> Int -> [Int]
performCalculationsPart2 l target =
  let strList = filter (not . null) (splitOn ' ' l)
      interleavedList = getInterleavedList strList operatorsPart2
   in map (calculatePart2 target) interleavedList

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
                      match = find (== n) $ performCalculationsPart1 l n
                   in not (null match)
              )
              formattedInput

  putStrLn $ "Part 1 answer (267566105056): " ++ show results

  putStrLn "\nDay 7, Part 2"

  let result =
        sum $
          map ((\v -> read v :: Int) . head) $
            filter
              ( \[targetTotal, l] ->
                  let n = read targetTotal :: Int
                      match = find (== n) $ performCalculationsPart2 l n
                   in not (null match)
              )
              formattedInput

  putStrLn $ "Part 2 answer (116094961956019): " ++ show result
