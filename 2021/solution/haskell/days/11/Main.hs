module Main (main) where

import Common
import Control.Monad.State
import Parsers

-- $setup
-- >>> let testInput = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

type EnegryState = (Int, Matrix Int)

type EnergyValue = Int

energyStep :: Int -> Int
energyStep i
  | i `elem` [0..8] = i + 1
  | otherwise = 0


-- | get valid points
--
-- >>> getValid (fromList 3 3 [1..]) (3, 2)
-- [(2,2),(3,1)]
getValid :: Matrix Int -> Point -> [Point]
getValid m (r, c) =
  let top = ((r - 1, c), safeGet (r - 1) c m)
      topRight = ((r, c + 1), safeGet r (c + 1) m)
      right = ((r, c + 1), safeGet r (c + 1) m)
      bottom = ((r + 1, c), safeGet (r + 1) c m)
      left = ((r, c - 1), safeGet r (c - 1) m)
   in map fst $ filter ((< Just 9) . snd) $ filter (isJust . snd) [top, right, bottom, left]

ripple :: Matrix Int -> Int -> Matrix Int


energyEffect :: Int -> State EnegryState EnergyValue
energyEffect 0 = gets snd
energyEffect st = do
  m <- get
  let first = mapPos (\(r, c) a -> energyStep $ a ! (r, c)) m
  let ripple = mapPos (\(r, c) a -> )

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
