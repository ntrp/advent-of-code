#!/usr/bin/env runhaskell

module Part1 where

import Common

solve :: [String] -> String
solve = show

main :: IO ()
main = interact $ solve . parse
