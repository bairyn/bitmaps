module Paths_bitmap_array (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/bob/.cabal/bin"
libdir     = "/home/bob/.cabal/lib/bitmap-array-0.2.0.0/ghc-6.12.3"
datadir    = "/home/bob/.cabal/share/bitmap-array-0.2.0.0"
libexecdir = "/home/bob/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bitmap_array_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bitmap_array_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bitmap_array_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bitmap_array_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
