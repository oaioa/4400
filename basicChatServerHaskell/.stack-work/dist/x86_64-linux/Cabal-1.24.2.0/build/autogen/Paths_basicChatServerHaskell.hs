{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_basicChatServerHaskell (
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

bindir     = "/root/server_project/4400/basicChatServerHaskell/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/bin"
libdir     = "/root/server_project/4400/basicChatServerHaskell/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2/basicChatServerHaskell-0.1.0.0"
dynlibdir  = "/root/server_project/4400/basicChatServerHaskell/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/root/server_project/4400/basicChatServerHaskell/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/share/x86_64-linux-ghc-8.0.2/basicChatServerHaskell-0.1.0.0"
libexecdir = "/root/server_project/4400/basicChatServerHaskell/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/libexec"
sysconfdir = "/root/server_project/4400/basicChatServerHaskell/.stack-work/install/x86_64-linux/lts-9.14/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "basicChatServerHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "basicChatServerHaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "basicChatServerHaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "basicChatServerHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "basicChatServerHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "basicChatServerHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
