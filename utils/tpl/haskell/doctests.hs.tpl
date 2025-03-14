{-# LANGUAGE BlockArguments #-}

import Data.Foldable
import Data.List
import System.Directory
import System.FilePath
import System.IO
import Test.DocTest
import Text.Printf

main :: IO ()
main =
  do
    for_ ["days/" ++ printf "%02d" x ++ "/Main.hs" | x <- [1 .. 1] :: [Int]] \file ->
      do
        hPutStrLn stderr ("Testing " ++ file)
        doctest ["-icommon", file]
