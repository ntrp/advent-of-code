module Main(main) where

import Common

-- $setup
-- >>> let testInput = "te\nst"

solve1 :: [Int] -> Int
solve1 []     = 0
solve1 [_]    = 1
solve1 xs = head [ x * y | x <- xs, y <- xs, x + y == 2020]

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "not implemented"
part1 :: [String] -> String
part1 = show . solve1 . toIntList

solve2 :: [Int] -> Int
solve2 []     = 0
solve2 [_]    = 1
solve2 xs = head [ x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = show . solve2 . toIntList

main :: IO ()
main = interact $ solution part1 part2
