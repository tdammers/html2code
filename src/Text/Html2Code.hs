module Text.Html2Code
( parseTree
, TagTree (..)
)
where

import qualified Data.Text as Text
import Data.Text (Text)
import Text.HTML.TagSoup.Tree (parseTree, TagTree (..))
