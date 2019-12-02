{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_a_watched_pot (
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

bindir     = "/home/cole/Sources/games/a_watched_pot/.cabal-sandbox/bin"
libdir     = "/home/cole/Sources/games/a_watched_pot/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/a-watched-pot-0.1.0.0-91a8bi4hHXL7LMI7uZzS0j"
dynlibdir  = "/home/cole/Sources/games/a_watched_pot/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/cole/Sources/games/a_watched_pot/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/a-watched-pot-0.1.0.0"
libexecdir = "/home/cole/Sources/games/a_watched_pot/.cabal-sandbox/libexec"
sysconfdir = "/home/cole/Sources/games/a_watched_pot/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "a_watched_pot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "a_watched_pot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "a_watched_pot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "a_watched_pot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "a_watched_pot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "a_watched_pot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
