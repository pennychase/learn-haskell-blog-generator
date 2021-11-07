module Main where

import Convert (process)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

confirm :: IO Bool
confirm =
  putStrLn "Do you want to overwrite? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n" *>
            confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

main :: IO ()
main = do
    args <- getArgs
    case args of
      -- No argumnents - read from stdin and write to stdout
      [] -> do
        contents <- getContents 
        putStrLn $ process "" contents
      -- input and output files
      [input, output] -> do
        contents <- readFile input
        let result = process input contents
        fileExists <- doesFileExist output
        if fileExists
          then whenIO confirm (writeFile output result)
          else writeFile output result
      _ -> putStrLn "Usage: runghc Main.hs -- [<input-file> <output-file>]"




