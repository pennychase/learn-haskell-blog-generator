module OptParse
  ( Options (..)
  , SingleInput (..)
  , SingleOutput (..)
  , parse
  ) where

import Data.Maybe (fromMaybe)
import Options.Applicative

-- Options data types

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

-- Convert Single File

-- Input File parser
pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
  where
    parser =
      strOption
        ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input file"
        )

-- Output File parser
pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        (  long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file"
        )

-- SingleInput parser
pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

-- SingleOutput parser
pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

-- ConvertSingle parser
pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput

-- Convert Directory

-- InputDir parser
pInputDir :: Parser FilePath
pInputDir = 
  strOption
    (  long "input"
    <> short 'i'
    <> metavar "DIR PATH"
    <> help "Input directory"
    )

-- OuputDir parser
pOutputDir :: Parser FilePath
pOutputDir = 
  strOption
    (  long "output"
    <> short 'o'
    <> metavar "DIR PATH"
    <> help "Output directory"
    )

-- ConvertDir parser
pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir

-- Command parser
pOptions :: Parser Options
pOptions = 
  subparser                                                                                                                                  
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
    <> command
       "convert-dir"
       ( info
         (helper <*> pConvertDir)
         (progDesc "Convert a directory of markup files to html")
        )
     )

-- Parser Info
opts :: ParserInfo Options
opts =
  info (helper <*> pOptions)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

-- Command Line Options Parser
parse :: IO Options
parse = execParser opts
