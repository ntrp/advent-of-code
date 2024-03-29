{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aoc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ntrp/_pws/advent-of-code/2020/haskell/.stack-work/install/x86_64-linux-tinfo6/ef710882cb468a01f353fb1e9de33d0e1c12751a2d1b87dc394e5f1a911ad0dd/8.8.4/bin"
libdir     = "/home/ntrp/_pws/advent-of-code/2020/haskell/.stack-work/install/x86_64-linux-tinfo6/ef710882cb468a01f353fb1e9de33d0e1c12751a2d1b87dc394e5f1a911ad0dd/8.8.4/lib/x86_64-linux-ghc-8.8.4/aoc-0.1.0.0-5Mlh6S9Lno9C4mb0OegL9v"
dynlibdir  = "/home/ntrp/_pws/advent-of-code/2020/haskell/.stack-work/install/x86_64-linux-tinfo6/ef710882cb468a01f353fb1e9de33d0e1c12751a2d1b87dc394e5f1a911ad0dd/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/ntrp/_pws/advent-of-code/2020/haskell/.stack-work/install/x86_64-linux-tinfo6/ef710882cb468a01f353fb1e9de33d0e1c12751a2d1b87dc394e5f1a911ad0dd/8.8.4/share/x86_64-linux-ghc-8.8.4/aoc-0.1.0.0"
libexecdir = "/home/ntrp/_pws/advent-of-code/2020/haskell/.stack-work/install/x86_64-linux-tinfo6/ef710882cb468a01f353fb1e9de33d0e1c12751a2d1b87dc394e5f1a911ad0dd/8.8.4/libexec/x86_64-linux-ghc-8.8.4/aoc-0.1.0.0"
sysconfdir = "/home/ntrp/_pws/advent-of-code/2020/haskell/.stack-work/install/x86_64-linux-tinfo6/ef710882cb468a01f353fb1e9de33d0e1c12751a2d1b87dc394e5f1a911ad0dd/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
