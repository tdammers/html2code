{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Text.Html2Code.Writers.GenericStructured
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

data Language a m =
    Language
        { lBeginTag :: QName -> W a m ()
        , lEndTag :: QName -> W a m ()
        , lText :: String -> W a m ()
        , lBeginAttribs :: W a m ()
        , lSepAttribs :: W a m ()
        , lEndAttribs :: W a m ()
        , lAttrib :: QName -> [a] -> W a m ()
        , lBeginChildren :: W a m ()
        , lSepChildren :: W a m ()
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
    tell "\n"
    forM_ children $ \child -> do
        lift $ indent
        lift $ sep
        treeW child
        lift $ tell "\n"

treeW :: (StringLike a, IsString a, Monoid a, Monad m)
      => XmlTree
      -> ReaderT (Language a m) (W a m) ()
treeW (NTree t []) =
    tagW t
treeW (NTree (XTag tagName attribs) children) = do
    l <- ask
    lift $ lBeginTag l tagName
    indentedT $ do
        lift $ lBeginAttribs l
        attribsW attribs
        lift $ lEndAttribs l
        lift $ lBeginChildren l
        treesW $ filter visible children
        lift $ lEndChildren l

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
attribsW [] =
    tell "\n"
attribsW (x:xs) = do
    l <- ask
    attribW x
    tell "\n"
    forM_ xs $ \x -> do
        lift indent
        lift $ lSepAttribs l
        attribW x
        lift $ tell "\n"

attribW :: (StringLike a, IsString a, Monoid a, Monad m)
        => XmlTree
        -> ReaderT (Language a m) (W a m) ()
attribW (NTree (XAttr name) values) = do
    l <- ask
    valueStrs <- forM values $ \case
        (NTree (XText value) _) ->
            pure (fromString value)
        x -> do
            lift $ warn $ "Invalid attribute value: " <> show x
            pure ""
    lift $ lAttrib l name valueStrs
attribW x =
    lift $ warn $ "Invalid attribute: " <> show x

visible :: XmlTree -> Bool
visible (NTree (XText _) _) = True
visible (NTree (XTag _ _) _) = True
visible (NTree _ []) = True
visible _ = False
