module Main where

import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Text.Regex.PCRE ((=~))

countXmas :: String -> Int
countXmas l = length $ concat (l =~ "XMAS" :: [[String]])

leftToRightDiagonals :: [String] -> [String]
leftToRightDiagonals [] = []
leftToRightDiagonals xs = [extractDiagonal xs i | i <- [0 .. (length xs - 1)]]
  where
    extractDiagonal xs start = [xs !! (i + start) !! i | i <- [0 .. (min (length xs - 1 - start) (length (head xs) - 1))]]

rightToLeftDiagonals :: [String] -> [String]
rightToLeftDiagonals [] = []
rightToLeftDiagonals xs = [extractDiagonal xs i | i <- [0 .. (length xs - 1)]]
  where
    extractDiagonal xs start = [xs !! (i + start) !! (length (head xs) - 1 - i) | i <- [0 .. min (length xs - 1 - start) (length (head xs) - 1)]]

safeGetList :: [a] -> Int -> Maybe a
safeGetList xs index
  | index < 0 || index >= length xs = Nothing
  | otherwise = Just (xs !! index)

safeGetChar :: String -> Int -> Maybe Char
safeGetChar xs index
  | index < 0 || index >= length xs = Nothing
  | otherwise = Just (xs !! index)

getCharAtPosition :: Char -> [String] -> Int -> Int -> Char
getCharAtPosition defaultValue list x y = case safeGetList list y of
  Just line -> fromMaybe defaultValue (safeGetChar line x)
  Nothing -> defaultValue

main :: IO ()
main = do
  putStrLn "Day 4, Part 1"

  contents <- readFile "./day4/input.txt"

  let contentLines = lines contents

      c1 = map countXmas contentLines
      c2 = map countXmas $ transpose contentLines
      c3 = map (countXmas . reverse) contentLines
      c4 = map (countXmas . reverse) $ transpose contentLines

      ltrDiagonals = tail $ leftToRightDiagonals contentLines ++ leftToRightDiagonals (transpose contentLines)
      rtlDiagonals = tail $ rightToLeftDiagonals contentLines ++ rightToLeftDiagonals (reverse $ transpose $ reverse contentLines)

      dc1 = map countXmas (ltrDiagonals ++ rtlDiagonals)
      dc3 = map (countXmas . reverse) (ltrDiagonals ++ rtlDiagonals)

      totalCount = sum (c1 ++ c2 ++ c3 ++ c4 ++ dc1 ++ dc3)

  putStrLn $ "Part 1 answer (2414): " ++ show totalCount

  putStrLn "\nDay 4, Part 2"

  let matrixSize = length contentLines

      xMatrix = map fst $ filter ((== 'A') . snd) $ zip [0 ..] $ concat contentLines
      yMatrix = map (`div` matrixSize) xMatrix

      aPositions = zipWith (\x y -> (x `mod` matrixSize, y)) xMatrix yMatrix
      getAtPosition = getCharAtPosition '.' contentLines

      xmasCrossesCount =
        length $
          filter
            ( \(x, y) ->
                let chars = [getAtPosition (x - 1) (y - 1), getAtPosition (x - 1) (y + 1), getAtPosition (x + 1) (y - 1), getAtPosition (x + 1) (y + 1)]
                    conditions =
                      [ chars == pattern
                        | pattern <-
                            [ ['M', 'M', 'S', 'S'],
                              ['S', 'M', 'S', 'M'],
                              ['S', 'S', 'M', 'M'],
                              ['M', 'S', 'M', 'S']
                            ]
                      ]
                 in or conditions
            )
            aPositions

  putStrLn $ "Part 2 answer (1871): " ++ show xmasCrossesCount
