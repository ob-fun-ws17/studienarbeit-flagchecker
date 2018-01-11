module Lib where

import System.Directory
import Data.List.Split
import Control.Monad (filterM)
-- listDir :: FilePath -> IO [FilePath]
-- listDir path = listDirectory path

getEnding :: FilePath -> String
getEnding file = last (splitOn "." file)

filterFileTyp :: String -> [FilePath] -> [FilePath]
filterFileTyp ending files = filter ((== ending) . getEnding) files

filterDirs :: [FilePath] -> IO [FilePath]
filterDirs files = filterM doesDirectoryExist files

wholeDirRecursive :: FilePath -> FilePath -> IO ()
wholeDirRecursive src dst =
  do
    files <- listDirectory src
    filtered <- filterM (fmap not . doesDirectoryExist) files
    createDirectoryIfMissing True dst
    mapM_ (`copyFile` dst) filtered
