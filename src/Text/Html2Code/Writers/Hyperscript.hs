{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Text.Html2Code.Writers.Hyperscript
where

import Data.Monoid ( Monoid (..), (<>) )
import Data.String ( IsString (..) )
import Text.StringLike ( StringLike (..) )
import Text.Html2Code.Writers.W
import qualified Text.Html2Code.Writers.GenericStructured as G
import Text.Html2Code.Writers.GenericStructured (Language (..))
import Control.Monad (forM_, forM)
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.QualifiedName

hyperscript :: (StringLike a, IsString a, Monoid a, Monad m)
        => Language a m
hyperscript = Language
    { lBeginTag = \name -> do
        tell $ "h(" <> fromString (quoteStr (qualifiedName name)) <> ", \n"
    , lEndTag = \name ->
        tell ")"
    , lText = \str -> do
        tell $ fromString (quoteStr str)
    , lBeginAttribs =
        tellLn "{ "
    , lSepAttribs =
        tell ", "
    , lEndAttribs =
        tellLn "}\n"
    , lAttrib = \name values -> do
        tell $ fromString (qualifiedName name)
        tell $ ": "
        tell . quoteStr . mconcat $ values
        
    , lBeginChildren =
        tellLn ", "
    , lSepChildren =
        tell ", "
    , lEndChildren =
        tellLn ")"
    }

write :: (StringLike a, IsString a, Monoid a, Monad m)
      => (String -> m ())
      -> [XmlTree]
      -> m a
write = G.write hyperscript

quoteStr :: (StringLike a, IsString a) => a -> a
quoteStr = fromString . show . toString
