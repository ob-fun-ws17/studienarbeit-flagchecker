-- | The lib of the project. Most of the work is done here.
module Lib where

import System.Directory
import Data.List.Split
import Control.Monad (filterM, when, forM_)
import Control.Applicative((<$>))
import Control.Exception(throw)

-- | Extracts the file extension from a path.
getEnding :: FilePath -> String
getEnding file = last (splitOn "." file)

-- | Extracts the last part of the path, so the actual filename.
getFileName :: FilePath -> String
getFileName path = last (splitOn "/" path)

-- | Filters the files without the proper ending.
filterFileTyp :: String -> [FilePath] -> [FilePath]
filterFileTyp ending files = filter ((== ending) . getEnding) files

-- | Only returns a list of the directories.
filterDirs :: [FilePath] -> IO [FilePath]
filterDirs files = filterM doesDirectoryExist files

-- | Copies the file to the specified location.
copyFileToDst :: FilePath -> FilePath -> IO ()
copyFileToDst src dst =
  do
    createDirectoryIfMissing True dst
    copyFile src (dst ++ "/" ++ (getFileName src))

-- -- | Copies a whole directory to the specified location. Doesn't work recursively right now.
-- copyDirToDst :: FilePath -> FilePath -> IO ()
-- copyDirToDst src dst =
--   do
--     files <- listDirectory src
--     filtered <- filterM (fmap not . doesDirectoryExist) files
--     folders <- filterM doesDirectoryExist files
--     --foldersAndNames = zip (map getFileName folders) folders
--     createDirectoryIfMissing True dst
--     mapM_ (`copyFileToDst` dst) filtered
--     forM_ folders $ \name -> do
--       let srcPath = src ++ "/" ++ name
--       let dstPath = dst ++ "/" ++ name
--       createDirectoryIfMissing True dstPath
--       copyDirToDst srcPath dstPath

-- | Inspired by StackOverflow. Copies a whole Dir. Destination directory will be created.
-- | Fails if the destination directory already exists.
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (doesFileOrDirectoryExist dst) $
    throw (userError "destination already exists")

  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src ++ "/" ++ name
    let dstPath = dst ++ "/" ++ name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath
  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r


-- | Copies all the files with a specific ending from src to dst.
filteredCopy :: FilePath -> FilePath -> String -> IO ()
filteredCopy src dst ending =
  do
    files <- listDirectory src
    mapM_ (`copyFileToDst` dst) (filterFileTyp ending files)

-- | Copies a file multiple times. It will be named after the iteration.
multiCopy :: FilePath -> FilePath -> Int -> Int -> IO ()
multiCopy src dst iteration goal =
  do
    if iteration < goal then do
      copyFileToDst src dst
      renameFile (dst ++ "/" ++ (getFileName src)) (dst ++ "/" ++ show iteration ++ "." ++ (getEnding src))
      multiCopy src dst (iteration + 1) goal
    else do
      return ()

-- | Makes multiple copies of the specified filetype. Absolutely unique feature.
massMultiCopy :: FilePath -> FilePath -> String -> Int -> IO ()
massMultiCopy src dst ending copyCount =
  do
    files <- listDirectory src
    let filtered = filterFileTyp ending files
    let subGoals = map (*copyCount) [1..]
    let iterationStart = map (subtract copyCount) subGoals
    let startAndEnd = zip iterationStart subGoals
    let dstList = repeat (dst ++ "/" ++ (getFileName src))
    let sourceAndDst = zip filtered dstList
    let tuplelist = zip sourceAndDst startAndEnd
    mapM_ multiCopyCaller tuplelist

-- | Helps to call multiCopy with the appropriate values.
multiCopyCaller :: ((FilePath, FilePath), (Int, Int)) -> IO ()
multiCopyCaller ((src, dst), (iteration, goal)) = multiCopy src dst iteration goal
