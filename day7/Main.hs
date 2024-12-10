{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (replicateM)
import Data.Foldable (find, foldl')
import Lib (splitOn)

-- This is the important part where all the operator combinations are generated lazily
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
getInterleavedList numbersList operatorsList =
  map
    ( map
        extractValue
        . interleave numbersList
    )
    $ getAllOperatorCombinations numbersList operatorsList

calculate :: ((Int, String) -> String -> (Int, String)) -> Int -> [String] -> Int
calculate reducerFn target = fst . foldl' reducerFn (0, "+")

performCalculations :: [String] -> (Int -> [String] -> Int) -> String -> Int -> [Int]
performCalculations operations calcFn numbersList targetNumber =
  let strList = filter (not . null) (splitOn ' ' numbersList)
      interleavedList = getInterleavedList strList operations
   in map (calcFn targetNumber) interleavedList

getResult :: [[String]] -> (String -> Int -> [Int]) -> Int
getResult input calcFn =
  sum $
    map ((\v -> read v :: Int) . head) $
      filter
        ( \[targetTotal, numbersList] ->
            let targetNumber = read targetTotal :: Int
                match = find (== targetNumber) $ calcFn numbersList targetNumber
             in not (null match)
        )
        input

reducerPart1 :: (Int, String) -> String -> (Int, String)
reducerPart1 (runningTotal, op) item = case item of
  "+" -> (runningTotal, "+")
  "*" -> (runningTotal, "*")
  n ->
    let num = read n :: Int
     in case op of
          "+" -> (runningTotal + num, op)
          "*" -> (runningTotal * num, op)

reducerPart2 :: (Int, String) -> String -> (Int, String)
reducerPart2 (runningTotal, op) item = case item of
  "+" -> (runningTotal, "+")
  "*" -> (runningTotal, "*")
  "||" -> (runningTotal, "||")
  n ->
    let num = read n :: Int
     in case op of
          "+" -> (runningTotal + num, op)
          "*" -> (runningTotal * num, op)
          "||" -> (read (show runningTotal ++ n), op)

main :: IO ()
main = do
  putStrLn "Day 7, Part 1"

  contents <- readFile "./day7/input.txt"

  let operations = lines contents
      formattedInput = map (splitOn ':') operations
      result = getResult formattedInput $ performCalculations ["+", "*"] $ calculate reducerPart1

  putStrLn $ "Part 1 answer (267566105056): " ++ show result

  putStrLn "\nDay 7, Part 2"

  let result = getResult formattedInput $ performCalculations ["+", "*", "||"] $ calculate reducerPart2

  putStrLn $ "Part 2 answer (116094961956019): " ++ show result
