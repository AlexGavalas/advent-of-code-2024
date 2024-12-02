module Helpers (isSorted, permutationsByRemovingOne) where

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_ : xs) = xs
removeAt n (x : xs) = x : removeAt (n - 1) xs

permutationsByRemovingOne :: [a] -> [[a]]
permutationsByRemovingOne [] = []
permutationsByRemovingOne list = [removeAt i list | i <- [0 .. length list - 1]]

isAscending :: (Ord b) => [b] -> Bool
isAscending line = all (uncurry (>=)) (zip line (tail line))

isDescending :: (Ord b) => [b] -> Bool
isDescending line = all (uncurry (<=)) (zip line (tail line))

isSorted :: (Ord b) => [b] -> Bool
isSorted line = isAscending line || isDescending line
