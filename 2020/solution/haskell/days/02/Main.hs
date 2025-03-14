module Main(main) where

import Common
import Parsers

data Entry = Entry {low :: Int, high :: Int, character :: Char, pass :: String} deriving (Show)

parser :: Parser Entry
parser = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return Entry{low = read low, high = read high, character = c, pass = s}

-- $setup
-- >>> let testInput = "te\nst"

valid1 :: Entry -> Bool
valid1 Entry{low = low, high = high, character = c, pass = pass} =
  let matches = length $ filter (==c) pass
   in matches >= low && matches <= high

solve1 :: [Entry] -> Int
solve1 = length . filter (==True) . map valid1

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "not implemented"
part1 :: [String] -> String
part1 = show . solve1 . parselist parser

valid2 :: Entry -> Bool
valid2 Entry{low = low, high = high, character = c, pass = pass} = (pass !! (low - 1) == c) /= (pass !! (high - 1) == c)

solve2 :: [Entry] -> Int
solve2 = length . filter (==True) . map valid2

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = show . solve2 . parselist parser

main :: IO ()
main = interact $ solution part1 part2
