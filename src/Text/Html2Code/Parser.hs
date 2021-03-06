module Text.Html2Code.Parser
( parseTree
)
where

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Arrow.ReadDocument
import Control.Arrow.ListArrow

-- | Parse HTML source into an XML tree.
parseTree :: String -> [XmlTree]
parseTree src =
    runLA hread src
