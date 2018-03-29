{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_insite (
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
version = Version [0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/martin/.cabal/bin"
libdir     = "/home/martin/.cabal/lib/x86_64-linux-ghc-8.0.1/insite-0.2-1HBGzDq59d373rcj0tmAgQ"
dynlibdir  = "/home/martin/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/martin/.cabal/share/x86_64-linux-ghc-8.0.1/insite-0.2"
libexecdir = "/home/martin/.cabal/libexec"
sysconfdir = "/home/martin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "insite_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "insite_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "insite_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "insite_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "insite_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "insite_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
