module Paths_Tables (
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

bindir     = "/home/nate/.cabal/bin"
libdir     = "/home/nate/.cabal/lib/x86_64-linux-ghc-7.6.3/Tables-0.1.0.0"
datadir    = "/home/nate/.cabal/share/x86_64-linux-ghc-7.6.3/Tables-0.1.0.0"
libexecdir = "/home/nate/.cabal/libexec"
sysconfdir = "/home/nate/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Tables_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Tables_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Tables_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Tables_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Tables_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
