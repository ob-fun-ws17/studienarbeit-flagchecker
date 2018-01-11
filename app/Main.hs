module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Exit
import System.Directory
import Lib

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  dir <- listDirectory "."
  cmd <- getLine
  case cmd of
    "filter" -> putStrLn $ unlines (filterFileTyp "txt" dir)
    otherwise -> exitSuccess


--import System.IO
--import Data.Char(toUpper)

--main :: IO ()
--main = do
--       inh <- openFile "input.txt" ReadMode
--       outh <- openFile "output.txt" WriteMode
--       mainloop inh outh
--       hClose inh
--       hClose outh

-- mainloop :: Handle -> Handle -> IO ()
-- mainloop inh outh =
--    do ineof <- hIsEOF inh
--       if ineof
--           then return ()
--           else do inpStr <- hGetLine inh
--                   hPutStrLn outh (map toUpper inpStr)
--                   mainloop inh outh
