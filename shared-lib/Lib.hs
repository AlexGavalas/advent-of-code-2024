module Lib (getCharAtPosition, splitOn) where

import Data.Maybe (fromMaybe)

splitOn :: Char -> String -> [String]
splitOn delimiter = foldr f [[]]
  where
    f c acc@(x : xs)
      | c == delimiter = [] : acc
      | otherwise = (c : x) : xs

getList :: [a] -> Int -> Maybe a
getList xs index
  | index < 0 || index >= length xs = Nothing
  | otherwise = Just (xs !! index)

getCharacter :: String -> Int -> Maybe Char
getCharacter xs index
  | index < 0 || index >= length xs = Nothing
  | otherwise = Just (xs !! index)

getCharAtPosition :: Char -> [String] -> Int -> Int -> Char
getCharAtPosition defaultValue list x y = case getList list y of
  Just line -> fromMaybe defaultValue (getCharacter line x)
  Nothing -> defaultValue
