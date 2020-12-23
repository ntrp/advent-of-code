module Common (
  module Common
) where

countTrees :: Int -> Int -> [[Char]] -> Int
countTrees dx dy treeMatrix =
  let idx = tail [(x * dx) `mod` length (head treeMatrix) | x <- [0..]]
      idy = tail [y * dy | y <- [0..length treeMatrix - 1], y * dy < length treeMatrix]
   in length $ filter (==True) $ zipWith (\x y -> treeMatrix !! y !! x == '#') idx idy

parse :: String -> [String]
parse = lines
