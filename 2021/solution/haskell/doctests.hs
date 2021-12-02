{-# language BlockArguments #-}
import Data.Foldable
import Data.List
import System.Directory
import System.FilePath
import System.IO
import Test.DocTest

main :: IO ()
main =
  do files <- processFiles <$> listDirectory "days"
     for_ files \file ->
       do hPutStrLn stderr ("Testing " ++ file)
          doctest ["-icommon", file]

processFiles :: [FilePath] -> [FilePath]
processFiles
  = map ("days"</>)
  . filter (\x -> "Day" `isPrefixOf` takeBaseName x && takeExtension x == ".hs")
  . sort
