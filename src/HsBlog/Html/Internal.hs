module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html = Html String

type Document = [Structure]

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = Structure ""

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        (el "head" (el "title" (escape title)))
        <> el "body" (getStructureString content)
    )

p_ :: String -> Structure
p_ = structure_ "p"

h_ :: Natural -> String -> Structure
h_ n = structure_ ("h" <> show n)

h1_ :: String -> Structure
h1_ = structure_ "h1"

ul_ :: [Structure] -> Structure
ul_ = __l_ "ul"

ol_ :: [Structure] -> Structure
ol_ = __l_ "ol"

code_ :: String -> Structure
code_ = structure_ "pre"

empty_ :: Structure
empty_ = Structure ""

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

convertStructure :: [Structure] -> Structure
convertStructure xs =
  case xs of
    [] -> empty_
    y : ys -> y <> convertStructure ys

-- * Render

render :: Html -> String
render (Html html) = html

-- * Utilities

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

structure_ :: String -> String -> Structure
structure_ tag = Structure . el tag . escape

__l_ :: String -> [Structure] -> Structure
__l_ tag = Structure . el tag . concat . map (el "li" . getStructureString)

getStructureString :: Structure -> String
getStructureString (Structure s) = s

escape :: String -> String
escape =
  let escapeChar c = case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
   in concat . map escapeChar
