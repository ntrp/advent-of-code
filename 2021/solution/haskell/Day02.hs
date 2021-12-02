#!/usr/bin/env runhaskell

module Day01 where

import Common

data Direction = Forward | Up | Down deriving (Eq, Ord, Enum, Show)
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

applyCommand :: (Int, Int) -> Command -> (Int, Int)
applyCommand (pos, depth) Command{direction=Forward, power=val} = (pos + val, depth)
applyCommand (pos, depth) Command{direction=Up, power=val} = (pos, depth - val)
applyCommand (pos, depth) Command{direction=Down, power=val} = (pos, depth + val)

computePositionFragments :: [Command] -> (Int, Int)
computePositionFragments = foldl (\state curr -> applyCommand state curr) (0, 0)

computePosition :: (Int, Int) -> Int
computePosition (a, b) = a * b

part1 :: [String] -> String
part1 = show . computePosition . computePositionFragments . parselist parser

part2 :: [String] -> String
part2 _ = "n/a"

main :: IO ()
main = interact $ solution part1 part2
