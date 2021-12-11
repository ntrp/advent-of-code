module Main (main) where

import Common
import Control.Monad.State
import Data.List
import Data.Matrix hiding (trace)
import Data.Maybe
import Debug.Trace
import Parsers

-- $setup
-- >>> let testInput = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

type Point = (Int, Int)

type EnegryState = (Int, Matrix Int)

type EnergyValue = Int

-- | Increase energy if not 0 or >= 9
--
-- >>> energyStep 0
-- 0
-- >>> energyStep 4
-- 5
-- >>> energyStep 9
-- 0
energyStep :: Int -> Int
energyStep i
  | i `elem` [1 .. 8] = i + 1
  | otherwise = 0

-- | get valid points
--
-- >>> length $ getValid (fromLists [[1,1,1],[1,1,0],[1,1,0]]) (3, 2)
-- 3
-- >>> length $ getValid (fromLists [[1,1,1],[1,1,0],[1,1,0]]) (1, 1)
-- 3
-- >>> length $ getValid (fromLists [[1,1,1],[1,1,0],[1,1,0]]) (3, 1)
-- 3
-- >>> length $ getValid (fromLists [[1,1,1],[1,1,0],[1,1,0]]) (1, 3)
-- 2
getValid :: Matrix Int -> Point -> [Point]
getValid m (r, c) =
  let top = ((r - 1, c), safeGet (r - 1) c m)
      topRight = ((r - 1, c + 1), safeGet (r - 1) (c + 1) m)
      right = ((r, c + 1), safeGet r (c + 1) m)
      bottomRight = ((r + 1, c + 1), safeGet (r + 1) (c + 1) m)
      bottom = ((r + 1, c), safeGet (r + 1) c m)
      bottomLeft = ((r + 1, c - 1), safeGet (r + 1) (c - 1) m)
      left = ((r, c - 1), safeGet r (c - 1) m)
      topLeft = ((r - 1, c - 1), safeGet (r - 1) (c - 1) m)
      around = [top, topRight, right, bottomRight, bottom, bottomLeft, left, topLeft]
   in map fst (filter ((< Just 10) . snd) $ filter ((> Just 0) . snd) $ filter (isJust . snd) around)

-- | propagate flash from point
--
-- >>> ripple (fromLists [[1,1,1],[1,9,10],[1,1,1]])
-- ┌       ┐
-- │ 2 3 3 │
-- │ 2 0 0 │
-- │ 2 3 3 │
-- └       ┘
ripple :: Matrix Int -> Matrix Int
ripple m =
  let idx = [(r, c) | c <- [1 .. ncols m], r <- [1 .. nrows m]]
      -- collect al points that need to flahs
      readyToFlash = filter (\x -> (m ! x) > 9) idx
      -- set all to 0
      flashMatrix = foldr (\p a -> setElem (energyStep (a ! p)) p a) m readyToFlash
      -- produce all pheriperal points affected by the flash
      pheriperal = concatMap (getValid flashMatrix) readyToFlash
      -- increment all points (might produce points at 10 which will flash in the next recursion of the ripple)
      res = foldr (\p a -> setElem ((a ! p) + 1) p a) flashMatrix pheriperal
   in -- if there were no flashing points we are done
      if not $ null readyToFlash
        then ripple res
        else res

-- | State evolution fn that increments all elements by 1 and then applies the ripple effect
-- | At every iternation calculantes the amount of squids that flashed
energyEffect :: Int -> State EnegryState EnergyValue
energyEffect 0 = gets fst
energyEffect step = do
  (s, m) <- get
  let first = foldr (\p m -> setElem ((m ! p) + 1) p m) m [(r, c) | c <- [1 .. ncols m], r <- [1 .. nrows m]]
  let rippled = ripple first
  put (s + (length . filter (== 0) . toList $ rippled), rippled)
  energyEffect (step - 1)

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "1656"
part1 :: [String] -> String
part1 = eval . parse' digitMatrixParser . unlines
  where
    eval (Right m) = show $ evalState (energyEffect 100) (0, m)
    eval (Left e) = show e

type EnegryStateSync = Matrix Int

-- | State evolution that calculates the same steps in the energyEffect but instead counting the flashed squids
-- | verifies the moment when they all sync
syncEffect :: Int -> State EnegryStateSync EnergyValue
syncEffect gen = do
  m <- get
  let first = foldr (\p m -> setElem ((m ! p) + 1) p m) m [(r, c) | c <- [1 .. ncols m], r <- [1 .. nrows m]]
  let rippled = ripple first
  if length (filter (== 0) $ toList rippled) == (ncols rippled * nrows rippled)
    then return gen
    else do
      put rippled
      syncEffect (gen + 1)

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "195"
part2 :: [String] -> String
part2 = eval . parse' digitMatrixParser . unlines
  where
    eval (Right m) = show $ evalState (syncEffect 1) m
    eval (Left e) = show e

main :: IO ()
main = interact $ solution part1 part2
