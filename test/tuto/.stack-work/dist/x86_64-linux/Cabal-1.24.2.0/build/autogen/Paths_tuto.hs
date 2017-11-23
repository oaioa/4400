{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_tuto (
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

bindir     = "/home/boss/CODE/haskell/4400/test/tuto/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/bin"
libdir     = "/home/boss/CODE/haskell/4400/test/tuto/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2/tuto-0.1.0.0-1HexxdJ2kYu8U2s0fMBLRJ"
dynlibdir  = "/home/boss/CODE/haskell/4400/test/tuto/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/boss/CODE/haskell/4400/test/tuto/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/share/x86_64-linux-ghc-8.0.2/tuto-0.1.0.0"
libexecdir = "/home/boss/CODE/haskell/4400/test/tuto/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/libexec"
sysconfdir = "/home/boss/CODE/haskell/4400/test/tuto/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tuto_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tuto_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tuto_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tuto_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tuto_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tuto_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
