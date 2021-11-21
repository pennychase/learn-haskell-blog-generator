{-# LANGUAGE QuasiQuotes #-}

module MarkupParsingSpec where

import Test.Hspec
import HsBlog.Markup
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple
    multiline

simple :: Spec
simple = do
    describe "Simple tests" $ do
      it "empty" $
        shouldBe
          (parse "")
          []

      it "paragraph" $
        shouldBe
          (parse "Hello")
          [Paragraph "Hello"]

      it "paragraphs" $
        shouldBe
          (parse "Hello\n\nWorld")
          [Paragraph "Hello",Paragraph "World"]

      it "header 1" $
        shouldBe
          (parse "* Header 1")
          [Header 1 "Header 1"]

      it "code" $
        shouldBe
          (parse "> main = putStrLn \"hello world!\"")
          [CodeBlock ["main = putStrLn \"hello world!\""]]

multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe
        (parse example3)
        example3Result
    it "example4" $
      shouldBe
        (parse example4)
        example4Result
        

example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]

example3Result :: Document
example3Result =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]

example4 :: String
example4 = [r|
* Compiling programs with ghc

Running ghc invokes the Glasgow Haskell Compiler (GHC),
and can be used to compile Haskell modules and programs into native
executables and libraries.

Create a new Haskell source file named hello.hs, and write
the following code in it:

> main = putStrLn "Hello, Haskell!"

Now, we can compile the program by invoking ghc with the file name:

> $ ghc hello.hs
> [1 of 1] Compiling Main             ( hello.hs, hello.o )
> Linking hello ...

GHC created the following files:

- hello.hi - Haskell interface file
- hello.o - Object file, the output of the compiler before linking
- hello (or hello.exe on Microsoft Windows) - A native runnable executable.
|]

example4Result :: Document
example4Result = 
  [ Header 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock ["main = putStrLn \"Hello, Haskell!\""]
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
  , CodeBlock ["$ ghc hello.hs","[1 of 1] Compiling Main             ( hello.hs, hello.o )","Linking hello ..."]
  , Paragraph "GHC created the following files:"
  , UnorderedList 
      [ "hello.hi - Haskell interface file"
      , "hello.o - Object file, the output of the compiler before linking"
      , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
      ]
  ]
