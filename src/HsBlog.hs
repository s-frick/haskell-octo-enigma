module HsBlog
  ( main,
    process,
  )
where

import Control.Monad (when)
import HsBlog.Convert (convert)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

-- NOTE:
-- <> - append function # forall a. Semigroup a => a -> a -> a

-- ✔️ If the user calls the program without arguments, we will read from the standard input, and write to the standard output
-- ✔️ If the user calls the program with two arguments, the first one will be the input file name, and the second one will be the output file name
-- ✔️ If the output file already exists, we'll ask the user if they want to overwrite the file
-- ✔️ On any other kind of input, we'll print a generic message explaining the proper usage
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents
      putStrLn $ process "stdin" content
    --
    input : output : _ -> do
      content <- readFile input
      exists <- doesFileExist output
      let writeResult = writeFile output $ process input content
      let confirmOverwrite o = confirm ("File " <> o <> " already exists. Do you want to overwrte it? (y/n)")
      if exists
        then do whenIO (confirmOverwrite output) writeResult
        else writeResult
    --
    [_] -> usage

-- parse document to markup, convert to Html and render to string
process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: String -> IO Bool
confirm question = do
  putStrLn question
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response. use y or n" *> confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  when result action

usage :: IO ()
usage = putStrLn "Usage: runghc main.hs [-- <input-file> <output-file>]"
