module Main (main) where

import Common
import Data.List (elemIndex, findIndex)
import Data.Maybe

-- $setup
-- >>> let testInput = "16,1,2,0,4,2,7,1,2,14"

-- | parse a comma separated int list
--
-- >>> parse' dataParser testInput
-- Right [16,1,2,0,4,2,7,1,2,14]
dataParser :: Parser [Int]
dataParser = map read <$> sepBy (many1 digit) (string ",")

-- | bruteforce fuel solving
--
-- >>> solveFuel $ fromRight [] $ parse' dataParser testInput
-- 2
solveFuel :: [Int] -> Int
solveFuel xs = 
  let fuels = map (\pos -> sum $ map (\c -> abs (pos - c)) xs) [minimum xs .. maximum xs]
   in fromMaybe (-1) $ elemIndex (minimum fuels) fuels
   --in fuels
   --in [minimum xs, maximum xs]

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "2"
part1 :: [String] -> String
part1 = show . solveFuel . fromRight [] . parse' dataParser . unlines

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = notImplemented

main :: IO ()
main = interact $ solution part1 part2
