module HsBlog.Convert where

import HsBlog.Env (Env(..))
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

-- | Convert our Markup Structure into the corresponding HTML Structure. 
--   Handles header, paragraph, ordered and unordered lists, and code blocks
convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Header n txt ->
      Html.h_ n $ Html.txt_ txt

    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list

    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)

-- | Convert a single Markup document into an Html document
convert :: Env -- ^ Environment passes blog name, stylesheet, etc.
        -> String -- ^ Title (by default the filepath)
        -> Markup.Document -- ^ Markup document
        -> Html.Html
convert env title doc = 
  let
    header = Html.title_ (eBlogName env <> " - " <> title)
              <> Html.stylesheet_ (eStylesheetPath env)
    article = foldMap convertStructure doc
    websiteTitle = Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
    body = websiteTitle <> article
  in Html.html_ header body
