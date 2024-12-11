module Main where

import Data.Foldable (find)
import Data.List (groupBy, nub, sortBy)
import Data.Maybe (fromMaybe)

type Antenna = (Int, Int, Char)

indexedList :: [a] -> [(Int, a)]
indexedList = zip [0 ..]

getItemAtPosition :: [Antenna] -> (Int, Int) -> Maybe Antenna
getItemAtPosition list (x, y) = find (\(x', y', _) -> x' == x && y' == y) list

getUniquePairs :: [a] -> [(a, a)]
getUniquePairs xs = [(x, y) | (i, x) <- zip [0 ..] xs, (j, y) <- zip [0 ..] xs, i < j]

getAdjacentLinePoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAdjacentLinePoints (x1, y1) (x2, y2) =
  let (dx, dy) = (x2 - x1, y2 - y1)
      gcdXY = gcd dx dy
      dx' = dx `div` gcdXY
      dy' = dy `div` gcdXY
   in [(x1 - dx', y1 - dy'), (x2 + dx', y2 + dy')]

getAllAdjacentLinePoints :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
getAllAdjacentLinePoints (x1, y1) (x2, y2) maxDistance =
  let (dx, dy) = (x2 - x1, y2 - y1)
      gcdXY = gcd dx dy
      dx' = dx `div` gcdXY
      dy' = dy `div` gcdXY
      backwardPoints = take maxDistance [(x1 - (dx' * n), y1 - (dy' * n)) | n <- [0 ..]]
      forwardPoints = take maxDistance [(x2 + (dx' * n), y2 + (dy' * n)) | n <- [0 ..]]
   in backwardPoints ++ forwardPoints

main :: IO ()
main =
  do
    putStrLn "Day 8, Part 1"

    contents <- readFile "./day8/input.txt"

    let nodes = lines contents
        indexedNodes = concatMap (\(y, row) -> map (\(x, c) -> (x, y, c)) row) $ indexedList $ map indexedList nodes
        antennas = filter (\(_, _, c) -> c /= '.') indexedNodes
        similarAntennas =
          groupBy (\(_, _, c1) (_, _, c2) -> c1 == c2) $
            sortBy (\(_, _, c1) (_, _, c2) -> compare c1 c2) antennas
        result =
          nub $
            filter (\(_, _, c) -> c /= '@') $
              foldl
                ( \acc group ->
                    let pairs = getUniquePairs group
                        pointsOnLine =
                          concatMap
                            ( \((x1, y1, _), (x2, y2, _)) ->
                                let [p1, p2] = getAdjacentLinePoints (x1, y1) (x2, y2)
                                    p1' = fromMaybe (-1, -1, '@') $ getItemAtPosition indexedNodes p1
                                    p2' = fromMaybe (-1, -1, '@') $ getItemAtPosition indexedNodes p2
                                 in [p1', p2']
                            )
                            pairs
                     in acc ++ pointsOnLine
                )
                []
                similarAntennas

    putStrLn $ "Part 1 answer (381): " ++ show (length result)

    putStrLn "\nDay 8, Part 2"

    let matrixSize = length nodes
        result =
          nub $
            filter (\(_, _, c) -> c /= '@') $
              foldl
                ( \acc group ->
                    let pairs = getUniquePairs group
                        pointsOnLine =
                          concatMap
                            ( \((x1, y1, _), (x2, y2, _)) ->
                                let points = getAllAdjacentLinePoints (x1, y1) (x2, y2) matrixSize
                                    getItems = map (fromMaybe (-1, -1, '@') . getItemAtPosition indexedNodes)
                                 in getItems points
                            )
                            pairs
                     in acc ++ pointsOnLine
                )
                []
                similarAntennas

    putStrLn $ "Part 2 answer (1184): " ++ show (length result)
