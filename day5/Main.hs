module Main where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x : xs)
      | c == delimiter = [] : acc
      | otherwise = (c : x) : xs

middleElement :: [a] -> Maybe a
middleElement xs
  | null xs = Nothing
  | otherwise = Just (xs !! midIndex)
  where
    midIndex = length xs `div` 2

main :: IO ()
main = do
  putStrLn "Day 5, Part 1"

  contents <- readFile "./day5/input.txt"

  let contentLines = lines contents
      orderingRules = takeWhile (not . null) contentLines
      rules = map (splitOn '|') orderingRules
      updates = map (splitOn ',') $ drop 1 $ dropWhile (not . null) contentLines
      sumMiddleElements l = sum $ map (read . fromMaybe "0" . middleElement) l
      validUpdates =
        filter
          ( \update ->
              all
                ( \item ->
                    let itemRules = filter (\[a, b] -> a == item || b == item) rules
                        itemsBefore = takeWhile (/= item) update
                        itemsAfter = drop 1 $ dropWhile (/= item) update
                        validBefore = all (`notElem` map last itemRules) itemsBefore
                        validAfter = all (`notElem` map head itemRules) itemsAfter
                     in validBefore && validAfter
                )
                update
          )
          updates
      total = sumMiddleElements validUpdates

  putStrLn $ "Part 1 answer (6384): " ++ show total

  putStrLn "\nDay 5, Part 2"

  let incorrectUpdates = filter (`notElem` validUpdates) updates
      correctList =
        sortBy
          ( \itemA itemB ->
              compare
                ( let itemRules = filter (\[a, b] -> a == itemA && b == itemB) rules
                      isAValid = (`notElem` map head itemRules) itemB
                   in isAValid
                )
                ( let itemRules = filter (\[a, b] -> a == itemB && b == itemA) rules
                      isBValid = (`notElem` map last itemRules) itemA
                   in isBValid
                )
          )
      correctedUpdates = map correctList incorrectUpdates
      total = sumMiddleElements correctedUpdates

  putStrLn $ "Part 2 answer (5353): " ++ show total
