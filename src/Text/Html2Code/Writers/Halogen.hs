{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Text.Html2Code.Writers.Halogen
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

halogen :: (StringLike a, IsString a, Monoid a, Monad m)
        => Language a m
halogen = Language
    { lBeginTag = \name -> do
        tell $ "HH." <> fromString (qualifiedName name) <> "\n"
    , lEndTag = \name ->
        pure ()
    , lText = \str -> do
        tell $ "HH.text " <> (quoteStr (fromString str))
    , lBeginAttribs =
        tellLn "( "
    , lSepAttribs =
        tell ", "
    , lEndAttribs =
        tellLn ")\n"
    , lAttrib = \name values -> do
        tell $ "HP."
        tell $ fromString (qualifiedName name)
        tell $ " "
        tell . quoteStr . mconcat $ values
        
    , lBeginChildren =
        tellLn "[ "
    , lSepChildren =
        tell ", "
    , lEndChildren =
        tellLn "]"
    }

write :: (StringLike a, IsString a, Monoid a, Monad m)
      => (String -> m ())
      -> [XmlTree]
      -> m a
write = G.write halogen

quoteStr :: (StringLike a, IsString a) => a -> a
quoteStr = fromString . show . toString
