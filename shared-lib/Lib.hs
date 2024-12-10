module Lib (splitOn) where

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x : xs)
      | c == delimiter = [] : acc
      | otherwise = (c : x) : xs
