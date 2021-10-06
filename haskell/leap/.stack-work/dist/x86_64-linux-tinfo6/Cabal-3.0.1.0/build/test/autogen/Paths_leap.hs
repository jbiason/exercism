{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_leap (
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
version = Version [1,6,0,10] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/var/home/jbiason/Projects/exercism/haskell/leap/.stack-work/install/x86_64-linux-tinfo6/fb971c408f03a4aa6eda736aa026e53de3540d66efc73bedb1ae6e06b3bc7296/8.8.4/bin"
libdir     = "/var/home/jbiason/Projects/exercism/haskell/leap/.stack-work/install/x86_64-linux-tinfo6/fb971c408f03a4aa6eda736aa026e53de3540d66efc73bedb1ae6e06b3bc7296/8.8.4/lib/x86_64-linux-ghc-8.8.4/leap-1.6.0.10-1OMYOoaCAf7BtVC4aNe49e-test"
dynlibdir  = "/var/home/jbiason/Projects/exercism/haskell/leap/.stack-work/install/x86_64-linux-tinfo6/fb971c408f03a4aa6eda736aa026e53de3540d66efc73bedb1ae6e06b3bc7296/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/var/home/jbiason/Projects/exercism/haskell/leap/.stack-work/install/x86_64-linux-tinfo6/fb971c408f03a4aa6eda736aa026e53de3540d66efc73bedb1ae6e06b3bc7296/8.8.4/share/x86_64-linux-ghc-8.8.4/leap-1.6.0.10"
libexecdir = "/var/home/jbiason/Projects/exercism/haskell/leap/.stack-work/install/x86_64-linux-tinfo6/fb971c408f03a4aa6eda736aa026e53de3540d66efc73bedb1ae6e06b3bc7296/8.8.4/libexec/x86_64-linux-ghc-8.8.4/leap-1.6.0.10"
sysconfdir = "/var/home/jbiason/Projects/exercism/haskell/leap/.stack-work/install/x86_64-linux-tinfo6/fb971c408f03a4aa6eda736aa026e53de3540d66efc73bedb1ae6e06b3bc7296/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "leap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "leap_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "leap_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "leap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "leap_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "leap_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
