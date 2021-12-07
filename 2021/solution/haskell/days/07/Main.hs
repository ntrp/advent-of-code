module Main (main) where

import Common

-- $setup
-- >>> let testInput = "16,1,2,0,4,2,7,1,2,14"

-- | parse a comma separated int list
--
-- >>> parse' dataParser testInput
-- Right [16,1,2,0,4,2,7,1,2,14]
dataParser :: Parser [Int]
dataParser = map read <$> sepBy (many1 digit) (string ",")

distance :: Int -> Int -> Int
distance pos i = abs (pos - i)

distanceQuad :: Int -> Int -> Int
distanceQuad pos i = dis $ abs (pos - i)
  where
    dis 0 = 0
    dis 1 = 1
    dis n = dis (n - 1) + n

-- | bruteforce fuel solving
--
-- >>> solveFuel distance $ fromRight [] $ parse' dataParser testInput
-- 37
solveFuel :: (Int -> Int -> Int) -> [Int] -> Int
solveFuel distanceFn xs = minimum $ map (\pos -> sum $ map (distanceFn pos) xs) [minimum xs .. maximum xs]

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "37"
part1 :: [String] -> String
part1 = show . solveFuel distance . fromRight [] . parse' dataParser . unlines

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "168"
part2 :: [String] -> String
part2 = show . solveFuel distanceQuad . fromRight [] . parse' dataParser . unlines

main :: IO ()
main = interact $ solution part1 part2
