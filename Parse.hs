module Parse where

import Data.Maybe

import Markup
import Euterpea.IO.MIDI.FromMidi2 (restructure)


parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context
   
    -- Header 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Header 1 (trim line) : parseLines Nothing rest)
   
    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) -> 
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- Code Block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)


    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
