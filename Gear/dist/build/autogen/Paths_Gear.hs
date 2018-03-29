{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Gear (
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
libdir     = "/home/martin/.cabal/lib/x86_64-linux-ghc-8.0.1/Gear-0.2-HYbY0JNbCMTB9FfxyCj8Z3"
dynlibdir  = "/home/martin/.cabal/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/martin/.cabal/share/x86_64-linux-ghc-8.0.1/Gear-0.2"
libexecdir = "/home/martin/.cabal/libexec"
sysconfdir = "/home/martin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gear_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gear_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Gear_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Gear_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gear_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gear_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
