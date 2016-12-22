{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_oglg_find (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dlc019/projects/math/OGL/stack-way/oglg-find/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/bin"
libdir     = "/home/dlc019/projects/math/OGL/stack-way/oglg-find/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/lib/x86_64-linux-ghc-8.0.1/oglg-find-0.1.0.0-6Z77diup4v5Drh54KnpzEY"
datadir    = "/home/dlc019/projects/math/OGL/stack-way/oglg-find/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/share/x86_64-linux-ghc-8.0.1/oglg-find-0.1.0.0"
libexecdir = "/home/dlc019/projects/math/OGL/stack-way/oglg-find/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/libexec"
sysconfdir = "/home/dlc019/projects/math/OGL/stack-way/oglg-find/.stack-work/install/x86_64-linux/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "oglg_find_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "oglg_find_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "oglg_find_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "oglg_find_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "oglg_find_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
