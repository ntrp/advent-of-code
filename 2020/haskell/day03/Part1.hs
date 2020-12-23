#!/usr/bin/env runhaskell

module Part1 where

import Common

solve :: [String] -> String
solve = show . countTrees 3 1

main :: IO ()
main = interact $ solve . parse
