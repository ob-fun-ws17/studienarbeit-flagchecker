module Lib where

import System.Directory
import Data.List.Split
import Control.Monad (filterM)
-- listDir :: FilePath -> IO [FilePath]
-- listDir path = listDirectory path

getEnding :: FilePath -> String
getEnding file = last (splitOn "." file)

getFileName :: FilePath -> String
getFileName path = last (splitOn "/" path)

filterFileTyp :: String -> [FilePath] -> [FilePath]
filterFileTyp ending files = filter ((== ending) . getEnding) files

filterDirs :: [FilePath] -> IO [FilePath]
filterDirs files = filterM doesDirectoryExist files

wholeDirRecursive :: FilePath -> FilePath -> IO ()
wholeDirRecursive src dst = -- does not work right now ___ dst name needs to be generated from src name + dst path
  do
    files <- listDirectory src
    filtered <- filterM (fmap not . doesDirectoryExist) files
    createDirectoryIfMissing True dst
    mapM_ (`copyFile` dst) filtered


copyFileToDst :: FilePath -> FilePath -> IO ()
copyFileToDst src dst =
  do
    createDirectoryIfMissing True dst
    copyFile src (dst ++ "/" ++ (getFileName src))

copyDirToDst :: FilePath -> FilePath -> IO ()
copyDirToDst src dst =
  do
    files <- listDirectory src
    filtered <- filterM (fmap not . doesDirectoryExist) files
    folders <- filterM doesDirectoryExist files
    --foldersAndNames = zip (map getFileName folders) folders
    createDirectoryIfMissing True dst
    mapM_ (`copyFileToDst` dst) filtered
    -- source = map (src ++ "/" ++) folders
    -- target = map (dst ++ "/" ++) folders
    -- mapM_ (copyDirToDst src dst) folders --foldersAndNames(sour)

filteredCopy :: FilePath -> FilePath -> String -> IO ()
filteredCopy src dst ending =
  do
    files <- listDirectory src
    mapM_ (`copyFileToDst` dst) (filterFileTyp ending files)

multiCopy :: FilePath -> FilePath -> Int -> Int -> IO ()
multiCopy src dst iteration goal =
  do
    if iteration < goal then do
      copyFileToDst src dst
      renameFile (dst ++ "/" ++ (getFileName src)) (dst ++ "/" ++ show iteration ++ "." ++ (getEnding src))
      multiCopy src dst (iteration + 1) goal
    else do
      return ()

-- multiCopyAssist :: FilePath -> FilePath -> (Int, Int) -> IO ()
-- multiCopyAssist src dst (iteration, goal) =
--   do
--     if iteration < goal then do
--       copyFileToDst src dst
--       renameFile (dst ++ "/" ++ (getFileName src)) (dst ++ "/" ++ show iteration ++ "." ++ (getEnding src))
--       multiCopyAssist src dst ((iteration + 1), goal)
--     else do
--       return ()

-- massMultiCopy :: FilePath -> FilePath -> String -> Int -> IO ()
-- massMultiCopy src dst ending copyCount =
--   do
--     files <- listDirectory src
--     filtered = filterFileTyp ending files
--     --subGoalSize = (length filtered) * copyCount
--     subGoals = [1..] * copyCount
--     fileAndGoal = zip subGoals filtered
--     filter (multicopy (src ++ "/" ++ getFileName) (dst ++ "/" ++ getFileName) ) fileAndGoal
--     mapM_ (multicopy src dst )
