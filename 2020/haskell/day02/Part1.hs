#!/usr/bin/env runhaskell

module Part1 where

import Common

valid :: Entry -> Bool
valid Entry{low = low, high = high, character = c, pass = pass} =
  let matches = length $ filter (==c) pass
   in matches >= low && matches <= high

solve :: [Entry] -> String
solve = show . length . filter (==True) . map valid

main :: IO ()
main = interact $ solve . parselist parser . lines
