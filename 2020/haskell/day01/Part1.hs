#!/usr/bin/env runhaskell

module Main where

solve :: [Int] -> Int
solve []     = 0
solve [_]    = 1
solve xs = head [ x * y | x <- xs, y <- xs, x + y == 2020]

main :: IO ()
main = interact $ show . solve . map read . words
