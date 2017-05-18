{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_rmathbk (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/media/home/darclaw/projects/math/math-repository/rmathbk/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/bin"
libdir     = "/media/home/darclaw/projects/math/math-repository/rmathbk/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/rmathbk-0.1"
dynlibdir  = "/media/home/darclaw/projects/math/math-repository/rmathbk/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/media/home/darclaw/projects/math/math-repository/rmathbk/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/share/x86_64-linux-ghc-8.0.2/rmathbk-0.1"
libexecdir = "/media/home/darclaw/projects/math/math-repository/rmathbk/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/libexec"
sysconfdir = "/media/home/darclaw/projects/math/math-repository/rmathbk/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rmathbk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rmathbk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rmathbk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rmathbk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rmathbk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rmathbk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
