#!/usr/bin/env stack
-- stack runghc

module Main where

import Common

valid :: Entry -> Bool
valid Entry{low = low, high = high, character = c, pass = pass} = (pass !! (low - 1) == c) /= (pass !! (high - 1) == c)

solve :: [Entry] -> String
solve = show . length . filter (==True) . map valid

main :: IO ()
main = interact $ solve . parselist parser . lines
