module Main where

import Text.Regex.PCRE ((=~))

main :: IO ()
main = do
  putStrLn "Day 3, Part 1"

  contents <- readFile "./day3/input.txt"

  let matches = contents =~ "mul\\(\\d+,\\d+\\)" :: [[String]]
      multiplications = map tail $ concatMap (\item -> item =~ "(\\d+),(\\d+)" :: [[String]]) (concat matches)
      result = sum $ map (\[x, y] -> read x * read y) multiplications

  putStrLn $ "Part 1 answer (182619815): " ++ show result

  putStrLn "\nDay 3, Part 2"

  let matches = contents =~ "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)" :: [[String]]
      isDont = (== "don't()")
      isDo = (== "do()")
      (_, multiplicationsRes) =
        foldl
          ( \(shouldCalc, acc) x -> do
              if isDont x
                then (False, acc)
                else
                  if isDo x
                    then (True, acc)
                    else do
                      let [[xS, yS]] = map tail (x =~ "(\\d+),(\\d+)" :: [[String]])
                          xN = read xS :: Int
                          yN = read yS :: Int
                      (shouldCalc, if shouldCalc then acc + (xN * yN) else acc)
          )
          (True, 0)
          $ concat matches

  putStrLn $ "Part 3 answer (80747545): " ++ show multiplicationsRes
