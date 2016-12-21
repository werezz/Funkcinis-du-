module Paths_tictactoe_online (
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

bindir     = "/Users/Povilas/.cabal/bin"
libdir     = "/Users/Povilas/.cabal/lib/x86_64-osx-ghc-7.10.3/tictactoe-online-0.1.0.0-AqpUyzzob5D6r0McQARBri"
datadir    = "/Users/Povilas/.cabal/share/x86_64-osx-ghc-7.10.3/tictactoe-online-0.1.0.0"
libexecdir = "/Users/Povilas/.cabal/libexec"
sysconfdir = "/Users/Povilas/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tictactoe_online_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tictactoe_online_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tictactoe_online_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tictactoe_online_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tictactoe_online_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
