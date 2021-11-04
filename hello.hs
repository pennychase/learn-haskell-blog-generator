module Main where

import Html ( Html, html_, p_, h1_, render )

myHtml1 :: Html
myHtml1 = html_ "Learn Haskell" $  (h1_ "Header1") <> (p_ "Paragraph1") <> (p_ "Paragraph2")
                                
main :: IO ()
main = putStrLn $ render myHtml1
