#!/usr/bin/env runhaskell

module Day01 where

import Common

toIntList :: [String] -> [Int]
toIntList = map read . filter (not . null)

computeCount :: [Int] -> (Int, Int)
computeCount (x:xs) = foldl (\(count, prev) curr -> (if curr > prev then count + 1 else count, curr)) (0, x) xs

part1 :: [String] -> String
part1 = show . fst . computeCount . toIntList

toSlidingWindow :: [Int] -> [Int]
toSlidingWindow [a] = []
toSlidingWindow [a, b] = []
toSlidingWindow [a, b, c] = [a + b + c]
toSlidingWindow (a:b:c:xs) = a + b + c : toSlidingWindow (b : c : xs)

part2 :: [String] -> String
part2 = show . fst . computeCount . toSlidingWindow . toIntList

main :: IO ()
main = interact $ solution part1 part2
