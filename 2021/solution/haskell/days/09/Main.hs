module Main (main) where

import Common
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Matrix
import Data.Maybe
import qualified Data.Set as Set
import Parsers

type Point = (Int, Int)

-- $setup
-- >>> let testInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

pointAnalyze :: Matrix Int -> Point -> (Point, Bool)
pointAnalyze m (r, c) =
  let val = m ! (r, c)
      top = safeGet (r - 1) c m
      right = safeGet r (c + 1) m
      bottom = safeGet (r + 1) c m
      left = safeGet r (c - 1) m
   in ((r, c), minimum (catMaybes [top, right, bottom, left]) > val)

-- | analize floor to sum the spots
--
-- >>> floorAnalyze $ fromRight (zero 2 2) $ parse' digitMatrixParser testInput
-- 15
floorAnalyze :: Matrix Int -> Int
floorAnalyze m =
  let idx = [(r, c) | c <- [1 .. ncols m], r <- [1 .. nrows m]]
   in sum $ map ((+ 1) . (m !) . fst) $ filter snd $ map (pointAnalyze m) idx

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "15"
part1 :: [String] -> String
part1 = showEither . fmap floorAnalyze . parse' digitMatrixParser . unlines

-- | get valid points
--
-- >>> getValid (fromList 3 3 [1..]) (3, 2)
-- [(2,2),(3,1)]
getValid :: Matrix Int -> Point -> [Point]
getValid m (r, c) =
  let top = ((r - 1, c), safeGet (r - 1) c m)
      right = ((r, c + 1), safeGet r (c + 1) m)
      bottom = ((r + 1, c), safeGet (r + 1) c m)
      left = ((r, c - 1), safeGet r (c - 1) m)
   in map fst $ filter ((< Just 9) . snd) $ filter (isJust . snd) [top, right, bottom, left]

-- | compute the basin size
--
-- >>> computeBasinSize (fromRight (zero 2 2) $ parse' digitMatrixParser testInput) Set.empty [(1,10)]
-- 9
computeBasinSize :: Matrix Int -> Set.Set Point -> [Point] -> Int
computeBasinSize m s [] = 0
computeBasinSize m s ((r, c) : xp) =
  if Set.notMember (r, c) s
    then 1 + computeBasinSize m (Set.insert (r, c) s) (xp ++ valid)
    else 0
  where
    valid = filter (not . (`elem` xp)) $ filter (`Set.notMember` s) $ getValid m (r, c)

basinAnalyze :: Matrix Int -> Int
basinAnalyze m =
  let idx = [(r, c) | c <- [1 .. ncols m], r <- [1 .. nrows m]]
      lpoints = map fst $ filter snd $ map (pointAnalyze m) idx
   in product $ take 3 $ sortBy (flip compare) $ map (computeBasinSize m Set.empty . (: [])) lpoints

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "1134"
part2 :: [String] -> String
part2 = show . basinAnalyze . fromRight (zero 0 0) . parse' digitMatrixParser . unlines

main :: IO ()
main = interact $ solution part1 part2
