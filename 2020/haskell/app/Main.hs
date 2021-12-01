module Main where

import           Data.List
import           Data.Maybe
import           Util

main :: IO ()
main = do
  content <- lines <$> getInput "src/input/day12.txt"
  let time = read $ head content :: Int
      busses = map (read :: String -> Int) $
        filter (/="x") $
        split ',' $
        last content
  let busTimes = map (\x -> (time `div` x) * x + x) busses
  let part1 = busses !! fromJust (elemIndex (minimum busTimes) busTimes) * (minimum busTimes - time)
  print $ "Part1 " ++ show part1
  let bussesData = map (\(f, s) -> (f `mod` read s :: Int, read s :: Int)) $ filter ((/="x") . snd) $ zip [0..] $ split ',' $ last content
  print bussesData

