module Paths_Main (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/boss/CODE/haskell/4400/chat/test-cabal/.cabal-sandbox/bin"
libdir     = "/home/boss/CODE/haskell/4400/chat/test-cabal/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/Main-0.1.0.0"
datadir    = "/home/boss/CODE/haskell/4400/chat/test-cabal/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/Main-0.1.0.0"
libexecdir = "/home/boss/CODE/haskell/4400/chat/test-cabal/.cabal-sandbox/libexec"
sysconfdir = "/home/boss/CODE/haskell/4400/chat/test-cabal/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Main_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Main_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Main_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Main_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Main_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
