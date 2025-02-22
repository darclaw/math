{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_goglm4z3 (
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

bindir     = "/home/darclaw/projects/math/OGL/goglm4z3/.stack-work/install/x86_64-linux-nopie/lts-8.2/8.0.2/bin"
libdir     = "/home/darclaw/projects/math/OGL/goglm4z3/.stack-work/install/x86_64-linux-nopie/lts-8.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/goglm4z3-0.1.0.0-PpXcM9zhE76KpKQN4wcQN"
dynlibdir  = "/home/darclaw/projects/math/OGL/goglm4z3/.stack-work/install/x86_64-linux-nopie/lts-8.2/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/darclaw/projects/math/OGL/goglm4z3/.stack-work/install/x86_64-linux-nopie/lts-8.2/8.0.2/share/x86_64-linux-ghc-8.0.2/goglm4z3-0.1.0.0"
libexecdir = "/home/darclaw/projects/math/OGL/goglm4z3/.stack-work/install/x86_64-linux-nopie/lts-8.2/8.0.2/libexec"
sysconfdir = "/home/darclaw/projects/math/OGL/goglm4z3/.stack-work/install/x86_64-linux-nopie/lts-8.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "goglm4z3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "goglm4z3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "goglm4z3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "goglm4z3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "goglm4z3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "goglm4z3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
