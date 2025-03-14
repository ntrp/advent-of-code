module Main(main) where

import Common

-- $setup
-- >>> let testInput = "te\nst"

countTrees :: Int -> Int -> [[Char]] -> Int
countTrees dx dy treeMatrix =
  let idx = tail [(x * dx) `mod` length (head treeMatrix) | x <- [0..]]
      idy = tail [y * dy | y <- [0..length treeMatrix - 1], y * dy < length treeMatrix]
   in length $ filter (==True) $ zipWith (\x y -> treeMatrix !! y !! x == '#') idx idy

solve1 :: [String] -> Int
solve1 = countTrees 3 1

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "not implemented"
part1 :: [String] -> String
part1 = show . solve1

solve2 :: [String] -> Int
solve2 treeMap = 
  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
   in product $ map (\(x,y) -> countTrees x y treeMap) slopes

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = show . solve2

main :: IO ()
main = interact $ solution part1 part2
