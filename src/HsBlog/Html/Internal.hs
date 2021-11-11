module HsBlog.Html.Internal where

import Numeric.Natural

-- Types

type Title = String

newtype Html = Html String

-- Content
newtype Content = Content String

instance Monoid Content where
    mempty = Content ""

instance Semigroup Content where
        struct1 <> struct2 = Content $ getContentString struct1  <> getContentString struct2

-- Structure
newtype Structure = Structure String 

instance Monoid Structure where
    mempty = Structure ""

instance Semigroup Structure where
        struct1 <> struct2 = Structure $ getStructureString struct1  <> getStructureString struct2

-- Utilities

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

-- HTML generators

-- Structure

html_ :: Title -> Structure -> Html
html_ title content = Html $ el "html"
                           $ el "head" 
                           $ el "title" (escape title) <> el "body" (getStructureString content)

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

-- Content
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

