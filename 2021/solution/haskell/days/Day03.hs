module Main (main) where

import Common
import Data.List

-- $setup
-- >>> let testInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

-- | convert an array of zero and ones to a decimal number
--
-- >>> binToDec [1,1,0,0,0,1,0,1,0,1,1,1]
-- 3159
binToDec :: [Int] -> Int
binToDec = sum . map (2 ^) . elemIndices 1 . reverse

-- | convert a Char into a String
--
-- >>> toStrStr 'a'
-- "a"
toStrStr :: Char -> String
toStrStr c = [c]

-- | convert an array of strings to an array of arrays of int
--
-- >>> convert ["101", "001"]
-- [[1,0,1],[0,0,1]]
convert :: [String] -> [[Int]]
convert = map (map $ read . toStrStr)

-- | compute an increment while calculating the predominant bit (0 or 1)
--
-- >>> computeIncrement 2 1
-- 3
-- >>> computeIncrement 2 0
-- 1
computeIncrement :: Int -> Int -> Int
computeIncrement acc curr = if curr == 0 then acc - 1 else acc + 1

-- | should return the next state
--
-- >>> computeState [0,1,0,-1,0] [1,1,0,0,1]
-- [1,2,-1,-2,1]
computeState :: [Int] -> [Int] -> [Int]
computeState = zipWith computeIncrement

-- | should produce an array where if the value is > 0 we have a 1 and if < 0 we have a 0
--
-- >>> computeGammaBin [[0,0,1,0,0], [1,1,1,1,0], [1,0,1,1,0]]
-- [1,0,1,1,0]
-- >>> computeGammaBin [[0,0,1,0,0,1,0], [1,1,1,1,0,0,1], [1,0,1,1,0,1,1]]
-- [1,0,1,1,0,1,1]
computeGammaBin :: [[Int]] -> [Int]
computeGammaBin input =
  let size = length $ head input
   in map (\x -> if x > 0 then 1 else 0) $ foldl computeState (replicate size 0) input

reverseBin :: [Int] -> [Int]
reverseBin = map (\x -> if x == 1 then 0 else 1)

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "198"
part1 :: [String] -> String
part1 input =
  let gammaBin = computeGammaBin $ convert input
      epsilon = binToDec $ reverseBin gammaBin
   in show $ binToDec gammaBin * epsilon

-- | calculate the result
--
-- >>> findValue (\x -> if x > -1 then 1 else 0) 0 [[1,0,1]]
-- [1,0,1]
--
-- >>> findValue (\x -> if x > -1 then 1 else 0) 0 [[1,0,1],[1,0,0],[0,0,1]]
-- [1,0,1]
--
-- >>> findValue (\x -> if x > -1 then 0 else 1) 0 [[1,0,1],[1,0,0],[0,0,1]]
-- [0,0,1]
findValue :: (Int -> Int) -> Int -> [[Int]] -> [Int]
findValue _ _ [last] = last
findValue selectFn pos xs =
  let selectCol = map (!! pos) xs
      selectBit = selectFn $ foldl computeIncrement 0 selectCol
   in findValue selectFn (pos + 1) $ filter (\x -> (x !! pos) == selectBit) xs

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "230"
part2 :: [String] -> String
part2 input =
  let converted = convert input
      ogr = binToDec $ findValue (\x -> if x > -1 then 1 else 0) 0 converted
      csr = binToDec $ findValue (\x -> if x > -1 then 0 else 1) 0 converted
   in show $ ogr * csr

main :: IO ()
main = interact $ solution part1 part2
