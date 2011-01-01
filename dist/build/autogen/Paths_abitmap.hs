module Paths_abitmap (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/bob/.cabal/bin"
libdir     = "/home/bob/.cabal/lib/abitmap-0.1.0.1/ghc-6.12.3"
datadir    = "/home/bob/.cabal/share/abitmap-0.1.0.1"
libexecdir = "/home/bob/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "abitmap_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "abitmap_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "abitmap_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "abitmap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
