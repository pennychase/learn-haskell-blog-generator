module Main where

import Html ( Html, append_, html_, p_, h1_, render )

myHtml1 :: Html
myHtml1 = html_ "Learn Haskell" (append_ (h1_ "Header1") 
                                                 (append_ (p_ "Paragraph1") (p_ "Paragraph2")))
main :: IO ()
main = putStrLn $ render myHtml1
