module Main(main) where

import Common

-- $setup
-- >>> let testInput = "te\nst"

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "not implemented"
part1 :: [String] -> String
part1 = notImplemented

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = notImplemented

main :: IO ()
main = interact $ solution part1 part2
