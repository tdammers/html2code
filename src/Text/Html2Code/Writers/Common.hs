{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Text.Html2Code.Writers.Common
( W (..)
, runW
, warn
, indent
, indented
, tell
, tellLn
)
where

import Text.HTML.TagSoup (Tag (..), Attribute (..))
import Text.HTML.TagSoup.Tree (TagTree (..))
import Data.Monoid ( Monoid (..), (<>) )
import Data.String ( IsString (..) )
import Text.StringLike ( StringLike (..) )
import Control.Monad.RWS

type W a m = RWST (String -> m ()) a Int m

runW :: (StringLike a, IsString a, Monoid a, Monad m)
      => (String -> m ())
      -> W a m b
      -> m a
runW writeWarning a =
    snd <$> execRWST a writeWarning 0

warn :: (Monoid a, Monad m) => String -> W a m ()
warn msg = do
    writeWarning <- ask
    lift $ writeWarning msg

indent :: (IsString a, Monoid a, Monad m)
       => W a m ()
indent = do
    n <- get
    tell $ mconcat (replicate n "  ")

indentMore :: (Monoid a, Monad m) => W a m ()
indentMore = modify succ

indentLess :: (Monoid a, Monad m) => W a m ()
indentLess = modify pred

indented :: (Monoid a, Monad m) => W a m x -> W a m x
indented a = indentMore *> a <* indentLess

tellLn :: (IsString a, Monoid a, Monad m) => a -> W a m ()
tellLn msg = do
    indent
    tell msg


