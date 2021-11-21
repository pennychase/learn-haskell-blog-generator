{- | Internal types and functions for HTML. Public interface is exported in Html

-}

module HsBlog.Html.Internal where

import Numeric.Natural

-- * Types for representing HTML

newtype Html = Html String

newtype Header = Header String

newtype Content = Content String

newtype Structure = Structure String 

-- Instances

-- Header
instance Semigroup Header where
    Header h1 <> Header h2 = Header (h1 <> h2)

instance Monoid Header where
    mempty = Header ""

-- Content
instance Semigroup Content where
    struct1 <> struct2 = Content $ getContentString struct1  <> getContentString struct2

instance Monoid Content where
    mempty = Content ""

-- Structure
instance Semigroup Structure where
    struct1 <> struct2 = Structure $ getStructureString struct1  <> getStructureString struct2

instance Monoid Structure where
    mempty = Structure ""


-- * HTML EDSL

html_ :: Header -> Structure -> Html
html_ (Header header) content = Html 
                              $ el "html"
                              $ el "head" header
                              <> el "body" (getStructureString content)

-- ** Construct @\<head\>@

title_ :: String -> Header
title_ = Header . el "title" . escape

stylesheet_ :: FilePath -> Header
stylesheet_ path =
  Header $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta_ :: String -> String -> Header
meta_ name content =
  Header $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

-- ** Construct @\<body\>@

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_  = Structure . el "ul" . concatMap (el "li" . getStructureString) 

ol_ :: [Structure] -> Structure
ol_  = Structure . el "ol" . concatMap (el "li" . getStructureString) 

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- ** Construct content within structures

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

-- * Utilities

escape :: String -> String
escape =
    let
        escapeChar c =
            case c of 
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _   -> [c]
    in 
        concat . map escapeChar

getContentString :: Content -> String 
getContentString content =
    case content of
        Content str -> str

getStructureString :: Structure -> String 
getStructureString structure =
    case structure of
        Structure str -> str

render :: Html -> String 
render html =
    case html of
        Html str -> str

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


