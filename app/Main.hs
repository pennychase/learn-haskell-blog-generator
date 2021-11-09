module Main where

import HsBlog
import qualified HsBlog.Html as Html
import OptParse
import System.IO


getInput :: SingleInput -> IO (Html.Title, Handle)
getInput input =
  case input of
    Stdin -> pure ("Untitled", stdin)
    InputFile fname -> (,) fname <$> openFile fname ReadMode 

getOutput :: SingleOutput -> IO Handle
getOutput output =
  case output of
    Stdout-> pure stdout
    OutputFile fname -> openFile fname WriteMode 

main :: IO ()
main = do
  args <- parse
  case args of
    ConvertSingle input output -> do
      (title, inFile) <- getInput input
      outFile <- getOutput output
      convertSingle title inFile outFile
      hClose inFile
      hClose outFile
    ConvertDir input output -> convertDirectory input output


