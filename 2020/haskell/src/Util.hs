module Util
    ( 
    getInput,
    split
    ) where

getInput :: FilePath -> IO String
getInput = readFile

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y)
  where (x,y) = span (/= d) s

