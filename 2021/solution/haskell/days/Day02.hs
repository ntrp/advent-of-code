module Main(main) where

import Common

-- $setup
-- >>> let testInput = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

data Direction = Forward | Up | Down deriving (Enum, Show)
data Command = Command {direction :: Direction, power :: Int} deriving (Show)

parseDirection :: String -> Direction
parseDirection "forward" = Forward
parseDirection "up" = Up
parseDirection "down" = Down

parser :: Parser Command
parser = do
  direction <- many1 letter
  char ' '
  power <- many1 digit
  return Command{direction = parseDirection direction, power = read power}

-- | should compute the correct state based on the command
--
-- >>> applyCommand (1, 1) Command{direction=Forward,power=1}
-- (2,1)
-- >>> applyCommand (1, 1) Command{direction=Up,power=2}
-- (1,-1)
-- >>> applyCommand (1, 1) Command{direction=Down,power=3}
-- (1,4)
applyCommand :: (Int, Int) -> Command -> (Int, Int)
applyCommand (pos, depth) Command{direction=Forward, power=val} = (pos + val, depth)
applyCommand (pos, depth) Command{direction=Up, power=val} = (pos, depth - val)
applyCommand (pos, depth) Command{direction=Down, power=val} = (pos, depth + val)

computePositionFragments :: [Command] -> (Int, Int)
computePositionFragments = foldl applyCommand (0, 0)

-- | should multiply position and depth
--
-- >>> computePosition (3, 5)
-- 15
computePosition :: (Int, Int) -> Int
computePosition (a, b) = a * b

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "150"
part1 :: [String] -> String
part1 = show . computePosition . computePositionFragments . parseList' parser

-- | should compute the correct state based on the command
--
-- >>> applyCommandP2 (1, 1, 1) Command{direction=Forward,power=1}
-- (2,1,2)
-- >>> applyCommandP2 (1, 1, 1) Command{direction=Up,power=2}
-- (1,-1,1)
-- >>> applyCommandP2 (1, 1, 1) Command{direction=Down,power=3}
-- (1,4,1)
applyCommandP2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
applyCommandP2 (pos, aim, depth) Command{direction=Forward, power=val} = (pos + val, aim, depth + (aim * val))
applyCommandP2 (pos, aim, depth) Command{direction=Up, power=val} = (pos, aim - val, depth)
applyCommandP2 (pos, aim, depth) Command{direction=Down, power=val} = (pos, aim + val, depth)

computePositionFragmentsP2 :: [Command] -> (Int, Int, Int)
computePositionFragmentsP2 = foldl applyCommandP2 (0, 0, 0)

-- | should multiply position and depth
--
-- >>> computePositionP2 (2, 1, 3)
-- 6
computePositionP2 :: (Int, Int, Int) -> Int
computePositionP2 (a, _, b) = a * b

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "900"
part2 :: [String] -> String
part2 = show . computePositionP2 . computePositionFragmentsP2 . parseList' parser

main :: IO ()
main = interact $ solution part1 part2
