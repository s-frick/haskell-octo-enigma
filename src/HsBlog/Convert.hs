module HsBlog.Convert where

import qualified HsBlog.Html as H
import qualified HsBlog.Markup as M

-- folds over type Document = [Structure]
-- Structure has an instance of Monoid
convert :: H.Title -> M.Document -> H.Html
convert title = H.html_ title . foldMap convertStructure

convertStructure :: M.Structure -> H.Structure
convertStructure struct =
  case struct of
    M.Heading n txt -> H.h_ n txt
    M.Paragraph p -> H.p_ p
    M.UnorderedList list -> H.ul_ $ map H.p_ list
    M.OrderedList list -> H.ol_ $ map H.p_ list
    M.CodeBlock list -> H.code_ (unlines list)
