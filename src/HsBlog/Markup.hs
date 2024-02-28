module HsBlog.Markup
  ( Document,
    Structure (..),
    parse,
  )
where

import Data.Maybe (maybeToList)
import Numeric.Natural
import Prelude hiding (even, odd, replicate)

type Document =
  [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    ('*' : ' ' : line) : rest -> maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    ('*' : '*' : ' ' : line) : rest -> maybe id (:) context (Heading 2 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : ' ' : line) : rest -> maybe id (:) context (Heading 3 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : '*' : ' ' : line) : rest -> maybe id (:) context (Heading 4 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : '*' : '*' : ' ' : line) : rest -> maybe id (:) context (Heading 5 (trim line) : parseLines Nothing rest)
    ('*' : '*' : '*' : '*' : '*' : '*' : ' ' : line) : rest -> maybe id (:) context (Heading 6 (trim line) : parseLines Nothing rest)
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) -> parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ -> maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) -> parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ -> maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock list) -> parseLines (Just (CodeBlock (list <> [trim line]))) rest
        _ -> maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)
    l : rest | trim l == "" ->
      case context of
        Nothing -> parseLines Nothing rest
        Just structure -> structure : parseLines Nothing rest
    line : rest -> case context of
      Just (Paragraph paragraph) -> parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
      _anythingElse -> maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words

-- exercies
-- Hello, World!
-- example1 :: Document
-- example1 =
--   [ Paragraph "Hello, World!"
--   ]
--
-- -- * Welcome
--
-- -- To thid tutorial about Haskell
-- example2 :: Document
-- example2 =
--   [ Heading 1 "Welcome",
--     Paragraph "To this tutorial about Haskell."
--   ]
--
-- -- Remember that multiple lines with no separation
-- -- are grouped together into a single paragraph
-- -- but list items remain separate.
-- --
-- -- # Item 1 of a list
-- -- # Item 2 of the same list
-- example3 :: Document
-- example3 =
--   [ Paragraph "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate.",
--     OrderedList
--       [ "Item 1 of a list",
--         "Item 2 of the same list"
--       ]
--   ]
--
-- -- * Compiling programs with ghc
--
-- --
-- -- Running ghc invokes the Glasgow Haskell Compiler (GHC),
-- -- and can be used to compile Haskell modules and programs into native
-- -- executables and libraries.
-- --
-- -- Create a new Haskell source file named hello.hs, and write
-- -- the following code in it:
-- --
-- -- > main = putStrLn "Hello, Haskell!"
-- --
-- -- Now, we can compile the program by invoking ghc with the file name:
-- --
-- -- > ➜ ghc hello.hs
-- -- > [1 of 1] Compiling Main             ( hello.hs, hello.o )
-- -- > Linking hello ...
-- --
-- -- GHC created the following files:
-- --
-- -- - hello.hi - Haskell interface file
-- -- - hello.o - Object file, the output of the compiler before linking
-- -- - hello (or hello.exe on Microsoft Windows) - A native runnable executable.
-- --
-- -- GHC will produce an executable when the source file satisfies both conditions:
-- --
-- -- # Defines the main function in the source file
-- -- # Defines the module name to be Main or does not have a module declaration
-- --
-- -- Otherwise, it will only produce the .o and .hi files.
-- example4 :: Document
-- example4 =
--   [ Heading 1 "Compiling programs with ghc",
--     Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries.",
--     Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:",
--     CodeBlock
--       ["main = putStrLn \"Hello, Haskell!\""],
--     Paragraph "Now, we can compile the program by invoking ghc with the file name:",
--     CodeBlock
--       [ "-> ghc hello.hs",
--         "[1 of 1] Compiling Main\t\t ( hello.hs, hello.o )",
--         "Linking hello ..."
--       ],
--     Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
--     OrderedList
--       [ "Defines the main function in the source file",
--         "Defines the module name to be Main or does not have a module declaration"
--       ],
--     Paragraph "Otherwise, it will only produce .o and .hi files."
--   ]
--
-- -- Recursion exercise
-- replicate :: Int -> a -> [a]
-- replicate x a | x <= 0 = []
-- replicate x a = a : replicate (x - 1) a
--
-- -- Power of Mutual Recursion
-- odd :: Int -> Bool
-- odd x | x == 0 = False
-- odd x = even (x - 1)
--
-- even :: Int -> Bool
-- even x | x == 0 = True
-- even x = odd (x - 1)
