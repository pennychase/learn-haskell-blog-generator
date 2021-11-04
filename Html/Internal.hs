module Html.Internal where

-- Types

newtype Html = Html String

newtype Structure = Structure String 

type Title = String

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


getStructureString :: Structure -> String 
getStructureString structure =
    case structure of
        Structure str -> str

render :: Html -> String 
render html =
    case html of
        Html str -> str

-- HTML generators

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content = Html $ el "html"
                           $ el "head" 
                           $ el "title" (escape title) <> el "body" (getStructureString content)

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_  = Structure . el "ul" . concatMap (el "li" . getStructureString) 

ol_ :: [Structure] -> Structure
ol_  = Structure . el "ol" . concatMap (el "li" . getStructureString) 

code_ :: String -> Structure
code_ = Structure . el "pre" . escape
