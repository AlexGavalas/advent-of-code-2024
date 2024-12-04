module Main where

import Text.Regex.PCRE ((=~))

isDont :: String -> Bool
isDont = (== "don't()")

isDo :: String -> Bool
isDo = (== "do()")

performCalculations :: (Bool, Int) -> String -> (Bool, Int)
performCalculations (shouldProcessItem, acc) item
  | isDont item = (False, acc)
  | isDo item = (True, acc)
  | otherwise =
      let [n1, n2] = concatMap (tail . map read) (item =~ "(\\d+),(\\d+)" :: [[String]])
       in (shouldProcessItem, acc + if shouldProcessItem then n1 * n2 else 0)

main :: IO ()
main = do
  putStrLn "Day 3, Part 1"

  contents <- readFile "./day3/input.txt"

  let matches = concat (contents =~ "mul\\(\\d+,\\d+\\)" :: [[String]])
      multiplications = map tail $ concatMap (\item -> item =~ "(\\d+),(\\d+)" :: [[String]]) matches
      result = sum $ map (\[x, y] -> read x * read y) multiplications

  putStrLn $ "Part 1 answer (182619815): " ++ show result

  putStrLn "\nDay 3, Part 2"

  let matches = concat (contents =~ "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)" :: [[String]])
      (_, result) = foldl performCalculations (True, 0) matches

  putStrLn $ "Part 3 answer (80747545): " ++ show result
