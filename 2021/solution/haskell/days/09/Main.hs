module Main (main) where

import Common
import Data.Matrix
import Data.Maybe

-- $setup
-- >>> let testInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

-- | parse data to matrix of Int
--
-- >>> fromRight (zero 2 2) $ parse' dataParser testInput
-- ┌                     ┐
-- │ 2 1 9 9 9 4 3 2 1 0 │
-- │ 3 9 8 7 8 9 4 9 2 1 │
-- │ 9 8 5 6 7 8 9 8 9 2 │
-- │ 8 7 6 7 8 9 6 7 8 9 │
-- │ 9 8 9 9 9 6 5 6 7 8 │
-- └                     ┘
dataParser :: Parser (Matrix Int)
dataParser = fromLists . map (map $ read . (: [])) <$> sepEndBy1 (many1 digit) eol

pointAnalyze :: Matrix Int -> (Int, Int) -> Int
pointAnalyze m (r, c) =
  let val = m ! (r, c)
      top = safeGet (r - 1) c m
      right = safeGet r (c + 1) m
      bottom = safeGet (r + 1) c m
      left = safeGet r (c - 1) m
   in minimum (catMaybes [top, right, bottom, left]) > val ? val + 1 :? 0

-- | analize floor to sum the spots
--
-- >>> floorAnalyzer $ fromRight (zero 2 2) $ parse' dataParser testInput
-- 15
floorAnalyzer :: Matrix Int -> Int
floorAnalyzer m =
  let idx = [(r, c) | c <- [1 .. ncols m], r <- [1 .. nrows m]]
   in sum $ map (pointAnalyze m) idx

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "not implemented"
part1 :: [String] -> String
part1 = showEither . fmap floorAnalyzer . parse' dataParser . unlines

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = notImplemented

main :: IO ()
main = interact $ solution part1 part2
