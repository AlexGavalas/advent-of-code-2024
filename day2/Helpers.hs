module Helpers (permutationsByRemovingOne) where

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_ : xs) = xs
removeAt n (x : xs) = x : removeAt (n - 1) xs

permutationsByRemovingOne :: [a] -> [[a]]
permutationsByRemovingOne [] = []
permutationsByRemovingOne list = [removeAt i list | i <- [0 .. length list - 1]]
