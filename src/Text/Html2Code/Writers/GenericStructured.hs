{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}

-- | A generic writer for structured DSLs. The general pattern is, roughly:
--
-- @
-- beginTag
--     beginAttribs attrib
--     sepAttribs attrib
--     ...
--     endAttribs
--     beginChildren {recurse}
--     sepChildren {recurse}
--     ...
--     endChildren
-- endTag
-- @
--
-- The generic writer is parametrized with a 'Language' data structure that
-- fills in the non-generic parts (and thus defined what tags, attributes,
-- etc., look like in the specific target language).
module Text.Html2Code.Writers.GenericStructured
( Language (..)
, write
)
where

import Data.Monoid ( Monoid (..), (<>) )
import Data.String ( IsString (..) )
import Text.StringLike ( StringLike (..) )
import Text.Html2Code.Writers.W
import Control.Monad (forM_, forM)
import Text.XML.HXT.DOM.TypeDefs
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.QualifiedName
import Control.Monad.Reader
import Data.Maybe (catMaybes)

-- | Specifies the non-generic details for a writer.
data Language a m =
    Language
        { -- | Given a tag name and attributes, start writing a tag
          lBeginTag :: QName -> [(String, String)] -> W a m ()
          -- | Given a tag name and attributes, finish writing a tag
        , lEndTag :: QName -> [(String, String)] -> W a m ()
          -- | Write a text node
        , lText :: String -> W a m ()
          -- | Begin an attribute list
        , lBeginAttribs :: W a m ()
          -- | Write an attribute separator
        , lSepAttribs :: W a m ()
          -- | End an attribute list
        , lEndAttribs :: W a m ()
          -- | Write one attribute (name, value)
        , lAttrib :: QName -> a -> W a m ()
          -- | Determine whether an attribute should be rendered in the
          -- attribute list
        , lAttribVisible :: XmlTree -> Bool
          -- | Start a child node list
        , lBeginChildren :: W a m ()
          -- | Write a child node separator
        , lSepChildren :: W a m ()
          -- | Finish a child node list
        , lEndChildren :: W a m ()
        }

write :: (StringLike a, IsString a, Monoid a, Monad m)
      => Language a m
      -> (String -> m ())
      -> [XmlTree]
      -> m a
write language writeWarning trees =
    runW writeWarning (runReaderT (treesW' (pure ()) trees) language)

treesW :: (StringLike a, IsString a, Monoid a, Monad m)
      => [XmlTree]
      -> ReaderT (Language a m) (W a m) ()
treesW trees = do
    l <- ask
    treesW' (lSepChildren l) trees

treesW' :: (StringLike a, IsString a, Monoid a, Monad m)
        => (W a m ())
        -> [XmlTree]
        -> ReaderT (Language a m) (W a m) ()
treesW' _ [] = return ()
treesW' sep (child:children) = do
    treeW child
    lift endl
    forM_ children $ \child -> do
        lift sep
        treeW child
        lift endl

treeW :: (StringLike a, IsString a, Monoid a, Monad m)
      => XmlTree
      -> ReaderT (Language a m) (W a m) ()
treeW (NTree t []) =
    tagW t
treeW (NTree (XTag tagName attribs) children) = do
    l <- ask
    strAttribs <- fmap catMaybes $ forM attribs $ \case
        (NTree (XAttr name) values) ->
            Just . (qualifiedName name,) <$> collapseAttribValues (lift . warn) values
        x -> do
            lift $ warn $ "Invalid attribute value item " <> show x
            pure Nothing

    lift $ lBeginTag l tagName strAttribs
    indentedT $ do
        lift $ lBeginAttribs l
        attribsW attribs
        lift $ lEndAttribs l
        lift $ lBeginChildren l
        treesW $ filter visible children
        lift $ lEndChildren l
    lift $ lEndTag l tagName strAttribs

tagW :: (StringLike a, IsString a, Monoid a, Monad m)
     => XNode
     -> ReaderT (Language a m) (W a m) ()
tagW node = do
    l <- ask
    case node of
        XText str ->
            lift $ lText l str
        x -> do
            lift $ warn $ "html2code: Don't know how to deal with tag: " <> (show x) <> "\n"
            lift $ lText l ""

attribsW :: (StringLike a, IsString a, Monoid a, Monad m)
         => [XmlTree]
         -> ReaderT (Language a m) (W a m) ()
attribsW xs = do
    l <- ask
    let xs' = filter (lAttribVisible l) xs
    attribsW' xs'

attribsW' :: (StringLike a, IsString a, Monoid a, Monad m)
         => [XmlTree]
         -> ReaderT (Language a m) (W a m) ()
attribsW' [] =
    lift endl
attribsW' (x:xs) = do
    l <- ask
    attribW x
    lift endl
    forM_ xs $ \x -> do
        lift $ lSepAttribs l
        attribW x
        lift endl

attribW :: (StringLike a, IsString a, Monoid a, Monad m)
        => XmlTree
        -> ReaderT (Language a m) (W a m) ()
attribW (NTree (XAttr name) values) = do
    l <- ask
    valueStr <- collapseAttribValues (lift . warn) values
    lift $ lAttrib l name valueStr
attribW x =
    lift $ warn $ "Invalid attribute: " <> show x

collapseAttribValues :: (Monad m, IsString a, Monoid a)
                     => (String -> m ())
                     -> [XmlTree]
                     -> m a
collapseAttribValues warn values =
     fmap mconcat $ forM values $ \case
        (NTree (XText value) _) ->
            pure (fromString value)
        x -> do
            warn $ "Invalid attribute value: " <> show x
            pure ""

visible :: XmlTree -> Bool
visible (NTree (XText _) _) = True
visible (NTree (XTag _ _) _) = True
visible (NTree _ []) = True
visible _ = False
