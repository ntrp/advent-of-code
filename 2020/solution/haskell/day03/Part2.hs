#!/usr/bin/env runhaskell

module Part2 where

import Common

solve :: [String] -> String
solve treeMap = 
  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
   in show $ product $ map (\(x,y) -> countTrees x y treeMap) slopes

main :: IO ()
main = interact $ solve . parse
