module Paths_mp5_scheme (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/pw/cs421su/assignments/mp5-scheme/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/bin"
libdir     = "/Users/pw/cs421su/assignments/mp5-scheme/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/lib/x86_64-osx-ghc-7.10.3/mp5-scheme-0.1.0.0-GSNKP4tmOsEDiaoo5y6iRS"
datadir    = "/Users/pw/cs421su/assignments/mp5-scheme/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/share/x86_64-osx-ghc-7.10.3/mp5-scheme-0.1.0.0"
libexecdir = "/Users/pw/cs421su/assignments/mp5-scheme/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/libexec"
sysconfdir = "/Users/pw/cs421su/assignments/mp5-scheme/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp5_scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp5_scheme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mp5_scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp5_scheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp5_scheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
