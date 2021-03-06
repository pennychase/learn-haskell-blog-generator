module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert, convertStructure)
import HsBlog.Env( Env(..))

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)
import Control.Monad.Reader (Reader, runReader, ask)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath (takeExtension, takeBaseName, (<.>), (</>), takeFileName)
import System.Directory (createDirectory, removeDirectoryRecursive, listDirectory, doesDirectoryExist, copyFile)
import qualified HsBlog.Html as Html


-- Types

data DirContents = 
  DirContents
  { dcFilesToProcess :: [(FilePath, String)]
  , dcFilesToCopy :: [FilePath]
  }

-- convertDirectory and helpers

-- Convert a directory by creating HTML files from .txt files
-- and copying all other files to the destination directory
-- May throw an exception on output directory creation
convertDirectory :: Bool -> Env -> FilePath -> FilePath -> IO ()
convertDirectory overwrite env inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit overwrite outputDir
  let
    outputHtmls = runReader (txtsToRenderedHtml filesToProcess) env
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

-- Return the input directory content
-- Partition the directory files into .txt (which will be converted to HTML)
-- and other files (which will be copied)
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }

--  Convert markup .txt files, build an index, and render as HTML
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
txtsToRenderedHtml txtFiles = do
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
  index <- (,) "index.html" <$> buildIndex txtOutputFiles
  htmlPages <- traverse convertFile txtOutputFiles
  pure $ map (fmap Html.render) (index : htmlPages)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) = 
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> Reader Env (FilePath, Html.Html)
convertFile (file, doc) = do
  env <- ask
  pure (file, convert env (takeBaseName file) doc)

-- Build the index page
buildIndex :: [(FilePath, Markup.Document)] -> Reader Env Html.Html
buildIndex files = do
  env <- ask
  pure $ Html.html_ 
    (  Html.title_ (eBlogName env)
    <> Html.stylesheet_ (eStylesheetPath env)
    )
    (  Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
    <> Html.h_ 2 (Html.txt_ "Posts")
    <> mconcat (map mkPreview files)
    )
  where
    mkPreview (file, doc) = 
      case doc of
        Markup.Header 1 header : article -> 
          Html.h_ 3 (Html.link_ file (Html.txt_ header))
          <> foldMap convertStructure (take 2 article)
          <> Html.p_ (Html.link_ file (Html.txt_ "..."))
        _   -> Html.h_ 3 (Html.link_ file (Html.txt_ file))

-- Output to directory

-- Create output directory or tertminate if unable to    
createOutputDirectoryOrExit :: Bool -> FilePath -> IO ()
createOutputDirectoryOrExit overwrite outputDir =
  whenIO
    (not <$> createOutputDirectory overwrite outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- Creates the output directory and returns Bool signifying if successful
-- Asks user to confirm overwirting an existing directory 
createOutputDirectory :: Bool -> FilePath -> IO Bool
createOutputDirectory overwrite dir = do
  dirExists <- doesDirectoryExist dir
  create <- if dirExists
            then do
              overwrite' <- 
                if overwrite then pure overwrite
                else confirm "Output directory exists. Overwrite?"
              when overwrite' (removeDirectoryRecursive dir)
              pure overwrite'
            else pure True
  when create (createDirectory dir)
  pure create

-- Copy files to directory (the files that aren't converted)
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

-- Write converted files to directory
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) = writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

-- IO work and error handling

-- apply an IO action to a list, using Either to handle success ot failure
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action files = do
  for files $ \file -> do
    maybeContent <- 
      catch (Right <$> action file)
            (\(SomeException e) -> do
                pure $ Left (displayException e)
              )
    pure (file, maybeContent)

-- filter unsuccesful operations on files and report errors to stderr
filterAndReportFailures :: [(a, Either String b)] -> IO [(a,b)]
filterAndReportFailures =
  foldMap $ \ (file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content -> pure [(file, content)]

-- Utilities

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()


