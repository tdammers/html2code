{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}

-- | Writer for Hyperscript. This should also work with virtualdom, which uses
-- Hyperscript's EDSL for defining virtual DOM nodes.
module Text.Html2Code.Writers.Hyperscript
( hyperscript
, write
)
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
import Data.Maybe (catMaybes)
import Data.Char (toLower)
import Data.List (intercalate)
import Safe (readMay)

-- | Language definition for Hyperscript
hyperscript :: (StringLike a, IsString a, Monoid a, Monad m)
        => Language a m
hyperscript = Language
    { lBeginTag = \name attrs -> do
        let selector = mconcat . catMaybes $
                [ Just (qualifiedName name)
                , ("#" <>) <$> lookup "id" attrs
                , mconcat . map ("." <>) . words <$> lookup "class" attrs
                ]
        echo $ "h(" <> fromString (quoteStr selector) <> ","
        endl
    , lEndTag = \name _ ->
        echo ")"
    , lText = \str -> do
        echo $ fromString (quoteStr str)
    , lBeginAttribs =
        echo "{ "
    , lSepAttribs =
        echo ", "
    , lEndAttribs =
        echo "}" >> endl
    , lAttrib = hyAttrib
    , lAttribVisible = \case
        (NTree (XAttr name) _) ->
            case qualifiedName name of
                "class" -> False
                "id" -> False
                _ -> True
        _ -> False

    , lBeginChildren =
        echo ", "
    , lSepChildren =
        echo ", "
    , lEndChildren =
        echo ""
    }

-- | Render a document as Hyperscript.
write :: (StringLike a, IsString a, Monoid a, Monad m)
      => (String -> m ())
      -> [XmlTree]
      -> m a
write = G.write hyperscript

-- | Quote a string literal for JavaScript
--
-- TODO: this currently assumes that the string literal quoting rules for
-- JavaScript are identical to Haskell's 'Show' semantics, which is not quite
-- correct, but will work for trivial cases.
quoteStr :: (StringLike a, IsString a) => a -> a
quoteStr = fromString . show . toString

-- | Write an attribute as Hyperscript
hyAttrib :: (StringLike a, IsString a, Monoid a, Monad m)
         => QName -> a -> W a m ()
hyAttrib name value = do
    echo $ fromString (qualifiedName name)
    echo $ ": "
    echo $ fromString . show . toString $ value
