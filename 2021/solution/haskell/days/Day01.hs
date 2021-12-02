module Main(main) where

import Common

-- $setup
-- >>> let testInput = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"

-- | should convert to list of ints, ignore empty lines
--
-- >>> toIntList ["1", "2", "", "3"]
-- [1,2,3]
toIntList :: [String] -> [Int]
toIntList = map read . filter (not . null)

-- | should count how may times the value increases in a list
--
-- >>> computeCount [1, 2, 3, 3, 4, 2, 5]
-- (4,5)
computeCount :: [Int] -> (Int, Int)
computeCount (x:xs) = foldl (\(count, prev) curr -> (if curr > prev then count + 1 else count, curr)) (0, x) xs

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "7"
part1 :: [String] -> String
part1 = show . fst . computeCount . toIntList

-- | should create sliding window sums or 3 elements and put them in a list
--
-- >>> toSlidingWindow [1, 2, 3, 2, 1, 4]
-- [6,7,6,7]
toSlidingWindow :: [Int] -> [Int]
toSlidingWindow [a] = []
toSlidingWindow [a, b] = []
toSlidingWindow [a, b, c] = [a + b + c]
toSlidingWindow (a:b:c:xs) = a + b + c : toSlidingWindow (b : c : xs)

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "5"
part2 :: [String] -> String
part2 = show . fst . computeCount . toSlidingWindow . toIntList

main :: IO ()
main = interact $ solution part1 part2
