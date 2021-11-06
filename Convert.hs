module Convert where

import qualified Html
import qualified Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Header n txt ->
      Html.h_ n txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)

concatStructure :: [Html.Structure] -> Html.Structure
concatStructure xs =
  case xs of
    [] -> Html.empty_
    (y:ys) -> y <> concatStructure ys

convertDocument :: Markup.Document -> Html.Structure
convertDocument doc = concatStructure $ map convertStructure doc