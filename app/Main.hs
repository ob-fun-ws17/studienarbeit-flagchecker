-- | Main module of the app.
module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Exit
import System.Directory
import Lib
import Data.List.Split

-- | Entry point for the application.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please enter a valid command."
  putStrLn "Type help for a list of available commands."
  cmd <- getLine
  let args = splitOn " " cmd
  let option = head args
  case (option) of
    "help" -> printOptions
    "file" -> copyFileToDst (args!!1) (args!!2)
    "folder" -> copyDir (args!!1) (args!!2)
    "filter" -> filteredCopy (args!!1) (args!!2) (args!!3)
    "multi" -> multi
    "massMulti" -> massMulti

-- | Helps to call the multi function.
multi :: IO ()
multi = do
  putStrLn "Enter file to copy:"
  src <- getLine
  putStrLn "Enter dst directory:"
  dst <- getLine
  putStrLn "Enter number of copies:"
  copies <- getLine
  let value = read copies :: Int
  multiCopy src dst 0 value

-- | Helps to call the massMultiCopy function.
massMulti :: IO ()
massMulti = do
  putStrLn "Enter src directory:"
  src <- getLine
  putStrLn "Enter dst directory:"
  dst <- getLine
  putStrLn "Enter ending to copy:"
  ending <- getLine
  putStrLn "Enter number of copies:"
  copies <- getLine
  let value = read copies :: Int
  massMultiCopy src dst ending value

-- | Prints available commands.
printOptions :: IO ()
printOptions = do
  putStrLn "file      - copy a file"
  putStrLn "usage:    file <src> <dst>"
  putStrLn "folder    - copy a folder"
  putStrLn "usage:    folder <src> <dst>"
  putStrLn "filter    - copy all files with a specific ending"
  putStrLn "usage:    filter <src> <dst> <ending>"
  putStrLn "multi     - copy a file multiple times"
  putStrLn "usage:    multi (will prompt you again)"
  putStrLn "massMulti - copy all files with a specific ending multiple times"
  putStrLn "usage:    massMulti (will prompt you again)"
