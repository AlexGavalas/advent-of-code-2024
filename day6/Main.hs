module Main where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

indexedList :: [a] -> [(Int, a)]
indexedList = zip [0 ..]

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
  putStrLn "Day 6, Part 1"

  contents <- readFile "./day6/input.txt"

  let initialMap = lines contents
      indexedPositions = concatMap (\(y, row) -> map (\(x, c) -> (y, x, c)) $ indexedList row) $ indexedList initialMap
      findCurrentPosition = find (\(_, _, c) -> c == '^')
      getNextPosition (y, x) direction = case direction of
        '^' -> (y - 1, x, getCharAtPosition 'E' initialMap x (y - 1))
        'v' -> (y + 1, x, getCharAtPosition 'E' initialMap x (y + 1))
        '<' -> (y, x - 1, getCharAtPosition 'E' initialMap (x - 1) y)
        '>' -> (y, x + 1, getCharAtPosition 'E' initialMap (x + 1) y)
        _ -> (-1, -1, ' ')
      getNextDirection c = case c of
        '^' -> '>'
        '>' -> 'v'
        'v' -> '<'
        '<' -> '^'
        _ -> ' '
      canMoveToNextPosition (_, _, pos) = pos /= '#'
      startingPosition = fromMaybe (-1, -1, ' ') $ findCurrentPosition indexedPositions
      move =
        iterate
          ( \((y, x, c), direction) ->
              let nextPosition = getNextPosition (y, x) direction
               in if canMoveToNextPosition nextPosition
                    then (nextPosition, direction)
                    else ((y, x, c), getNextDirection direction)
          )
          (startingPosition, '^')
      walkedPath = takeWhile (\((y, x, c), direction) -> c /= 'E') move
      uniquePosiions = Set.fromList $ map ((\(y, x, _) -> (y, x)) . fst) walkedPath
      total = length uniquePosiions

  putStrLn $ "Part 1 answer (5080): " ++ show total

  -- putStrLn "\nDay 6, Part 2"

  -- putStrLn $ "Part 2 answer (): "
