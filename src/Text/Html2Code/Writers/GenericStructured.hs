{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}
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
import Data.Maybe (catMaybes)

data Language a m =
    Language
        { lBeginTag :: QName -> [(String, String)] -> W a m ()
        , lEndTag :: QName -> [(String, String)] -> W a m ()
        , lText :: String -> W a m ()
        , lBeginAttribs :: W a m ()
        , lSepAttribs :: W a m ()
        , lEndAttribs :: W a m ()
        , lAttrib :: QName -> a -> W a m ()
        , lAttribVisible :: XmlTree -> Bool
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
