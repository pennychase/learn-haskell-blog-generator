module Main where

import HsBlog
import qualified HsBlog.Html as Html
import OptParse

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO


getInput :: SingleInput -> IO (Html.Title, Handle)
getInput input =
  case input of
    Stdin -> pure ("", stdin)
    InputFile fname -> (,) fname <$> openFile fname ReadMode 

getOutput :: Bool -> SingleOutput -> IO Handle
getOutput overwrite output = 
  case output of
    Stdout-> pure stdout
    OutputFile fname -> do
      fileExists <- doesFileExist fname
      if fileExists
        then if overwrite
          then openFile fname WriteMode 
          else exitFailure
        else openFile fname WriteMode

main :: IO ()
main = do
  Opts (Overwrite overwrite) command <- parse
  case command of
    ConvertSingle input output -> do
      (title, inFile) <- getInput input
      outFile <- getOutput overwrite output
      convertSingle title inFile outFile
      hClose inFile
      hClose outFile
    ConvertDir input output -> convertDirectory overwrite input output


