{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Text.Html2Code.Writers.Hyperscript
where

import Text.HTML.TagSoup (Tag (..), Attribute (..))
import Text.HTML.TagSoup.Tree (TagTree (..))
import Data.Monoid ( Monoid (..), (<>) )
import Data.String ( IsString (..) )
import Text.StringLike ( StringLike (..) )
import Text.Html2Code.Writers.Common
import Control.Monad (forM_)

write :: (StringLike a, IsString a, Monoid a, Monad m) => (String -> m ()) -> [TagTree a] -> m a
write writeWarning trees =
    runW writeWarning (treesW' "" trees)

treesW :: (StringLike a, IsString a, Monoid a, Monad m)
      => [TagTree a]
      -> W a m ()
treesW = treesW' ", "

treesW' :: (StringLike a, IsString a, Monoid a, Monad m)
        => a
        -> [TagTree a]
        -> W a m ()
treesW' _ [] = return ()
treesW' sep (child:children) = do
    treeW child
    tell "\n"
    forM_ children $ \child -> do
        indent
        tell sep
        treeW child
        tell "\n"

treeW :: (StringLike a, IsString a, Monoid a, Monad m)
      => TagTree a
      -> W a m ()
treeW (TagLeaf tag) =
    tagW tag
treeW (TagBranch tagName attribs children) = do
    tell $ "h." <> tagName <> "(\n"
    indented $ do
        tellLn "  { "
        indented $ do
            attribsW attribs
            tellLn "}\n"
        tellLn ", [ "
        indented $ do
            treesW $ filter visible children
            tellLn "]\n"
        tellLn ")"

tagW :: (StringLike a, IsString a, Monoid a, Monad m)
     => Tag a
     -> W a m ()
tagW = \case 
    TagText str ->
        tell $ "h.text(" <> (quoteStr str) <> ")"
    x -> do
        warn $ "/* html2code: Don't know how to deal with tag: " <> (show . fmap toString $ x) <> " */\n"
        tell $ "h.text('')"

attribsW :: (StringLike a, IsString a, Monoid a, Monad m)
         => [Attribute a]
         -> W a m ()
attribsW [] =
    tell "\n"
attribsW (x:xs) = do
    attribW x
    tell "\n"
    forM_ xs $ \x -> do
        indent
        tell ", "
        attribW x
        tell "\n"

attribW :: (StringLike a, IsString a, Monoid a, Monad m)
        => Attribute a
        -> W a m ()
attribW (name, value) = do
    tell name
    tell ": "
    tell $ quoteStr value

visible :: TagTree a -> Bool
visible (TagBranch _ _ _) = True
visible (TagLeaf (TagText _)) = True
visible _ = False

quoteStr :: (StringLike a, IsString a) => a -> a
quoteStr = fromString . show . toString
