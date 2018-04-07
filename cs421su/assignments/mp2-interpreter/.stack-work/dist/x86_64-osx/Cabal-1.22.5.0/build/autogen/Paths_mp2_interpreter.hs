module Paths_mp2_interpreter (
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

bindir     = "/Users/pw/cs421su/assignments/mp2-interpreter/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/bin"
libdir     = "/Users/pw/cs421su/assignments/mp2-interpreter/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/lib/x86_64-osx-ghc-7.10.3/mp2-interpreter-0.1.0.0-Hrc7nkaSrWA9YcKkOUp4FD"
datadir    = "/Users/pw/cs421su/assignments/mp2-interpreter/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/share/x86_64-osx-ghc-7.10.3/mp2-interpreter-0.1.0.0"
libexecdir = "/Users/pw/cs421su/assignments/mp2-interpreter/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/libexec"
sysconfdir = "/Users/pw/cs421su/assignments/mp2-interpreter/.stack-work/install/x86_64-osx/lts-6.4/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp2_interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp2_interpreter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mp2_interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp2_interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp2_interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
