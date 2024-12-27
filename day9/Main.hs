module Main where

import Data.List (group)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

indexedList :: [a] -> [(Int, a)]
indexedList = zip [0 ..]

swapElements :: Int -> Int -> V.Vector a -> V.Vector a
swapElements i j =
  V.modify
    ( \v -> do
        x <- MV.read v i
        y <- MV.read v j
        MV.write v i y
        MV.write v j x
    )

main :: IO ()
main =
  do
    putStrLn "Day 9, Part 1"

    contents <- readFile "./day9/input.txt"

    let diskMap = concatMap (indexedList . map (map read . chunksOf 1) . chunksOf 2) $ lines contents
        expandedDiskMap =
          concatMap
            ( \(id, chars) -> case chars of
                [x, y] -> replicate x (show id) ++ replicate y "."
                [x] -> replicate x (show id)
                _ -> []
            )
            diskMap
        indexedExpandedDiskMap = V.indexed $ V.fromList expandedDiskMap
        denseDiskMap =
          V.foldl'
            ( \acc (currentIndex, item) ->
                if item == "."
                  then
                    let nextItem = V.find (\(_, item) -> item /= ".") $ V.reverse $ V.drop currentIndex acc
                     in case nextItem of
                          Just (nextItemIndex, _) -> swapElements currentIndex nextItemIndex acc
                          Nothing -> acc
                  else acc
            )
            indexedExpandedDiskMap
            indexedExpandedDiskMap
        checksum =
          V.ifoldl' (\acc id item -> if item == "." then acc else acc + id * read item) 0 $
            V.map snd denseDiskMap

    print $ "Part 1 answer (6346871685398): " ++ show checksum

    putStrLn "\nDay 9, Part 2"

    let defragmentedDiskMap =
          V.foldr
            ( \sublist acc ->
                if all (== ".") sublist
                  then acc
                  else
                    let emptySlotIndex = V.findIndex (\sl -> all (== ".") sl && length sl >= length sublist) $ V.takeWhile (/= sublist) acc
                        currentIndex = fromMaybe 0 $ V.findIndex (== sublist) acc
                     in case emptySlotIndex of
                          Just idx ->
                            let emptySublist = acc V.! idx
                                updateA =
                                  V.update
                                    acc
                                    ( V.singleton
                                        ( idx,
                                          take (length emptySublist) (sublist ++ emptySublist)
                                        )
                                    )
                                updateB =
                                  V.update
                                    updateA
                                    ( V.singleton
                                        ( currentIndex,
                                          take (length sublist) (emptySublist ++ sublist)
                                        )
                                    )
                             in V.fromList $ group $ concat $ V.toList updateB
                          Nothing -> acc
            )
            (V.fromList $ group expandedDiskMap)
            (V.fromList $ group expandedDiskMap)
        checksum =
          V.ifoldl' (\acc id item -> if item == "." then acc else acc + id * read item) 0 $
            V.fromList $
              concat defragmentedDiskMap

    putStrLn $ "Part 2 answer (6373055193464): " ++ show checksum
