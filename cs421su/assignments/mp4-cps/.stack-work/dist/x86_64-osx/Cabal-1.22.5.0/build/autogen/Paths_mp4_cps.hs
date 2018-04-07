module Paths_mp4_cps (
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

bindir     = "/Users/pw/cs421su/assignments/mp4-cps/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/bin"
libdir     = "/Users/pw/cs421su/assignments/mp4-cps/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/lib/x86_64-osx-ghc-7.10.3/mp4-cps-0.1.0.0-JP9pB2Srag7LiXuSclPuYO"
datadir    = "/Users/pw/cs421su/assignments/mp4-cps/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/share/x86_64-osx-ghc-7.10.3/mp4-cps-0.1.0.0"
libexecdir = "/Users/pw/cs421su/assignments/mp4-cps/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/libexec"
sysconfdir = "/Users/pw/cs421su/assignments/mp4-cps/.stack-work/install/x86_64-osx/lts-6.7/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp4_cps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp4_cps_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mp4_cps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp4_cps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp4_cps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
