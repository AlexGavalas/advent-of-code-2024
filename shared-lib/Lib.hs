module Lib (readFileByLine) where

readFileByLine :: FilePath -> IO String
readFileByLine = Prelude.readFile
