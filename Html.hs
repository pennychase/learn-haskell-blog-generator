module Html
    (
      Html
    , Title
    , Structure
    , html_
    , p_
    , h1_
    , append_
    , render
    ) where

-- Types

newtype Html = Html String

newtype Structure = Structure String 

type Title = String

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

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure $ str1 <> str2

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

