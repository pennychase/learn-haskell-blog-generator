module HsBlog
  ( process
  , convertSingle
  , convertDirectory
  , buildIndex
  ) where

import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

import HsBlog.Directory (convertDirectory, buildIndex)
import System.IO

process :: Html.Title -> String -> String 
process title = Html.render . convert title . Markup.parse

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)




