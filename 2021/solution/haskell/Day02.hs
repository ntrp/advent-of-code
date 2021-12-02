#!/usr/bin/env runhaskell

module Day01 where

import Common

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

applyCommandP2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
applyCommandP2 (pos, aim, depth) Command{direction=Forward, power=val} = (pos + val, aim, depth + (aim * val))
applyCommandP2 (pos, aim, depth) Command{direction=Up, power=val} = (pos, aim - val, depth)
applyCommandP2 (pos, aim, depth) Command{direction=Down, power=val} = (pos, aim + val, depth)

computePositionFragmentsP2 :: [Command] -> (Int, Int, Int)
computePositionFragmentsP2 = foldl (\state curr -> applyCommandP2 state curr) (0, 0, 0)

computePositionP2 :: (Int, Int, Int) -> Int
computePositionP2 (a, _, b) = a * b

part2 :: [String] -> String
part2 = show . computePositionP2 . computePositionFragmentsP2 . parselist parser

main :: IO ()
main = interact $ solution part1 part2
