{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}

-- | An abstracted monad that provides the features we need for a code echor,
-- without caring about their specific implementations. Those features are
-- sending code to a target (\"writing\"), raising warnings, and tracking
-- current indentation level.
module Text.Html2Code.Writers.W
( W (..)
, runW
, warn
, indented
, indentedT
, echo
, endl
)
where

import Text.HTML.TagSoup (Tag (..), Attribute (..))
import Text.HTML.TagSoup.Tree (TagTree (..))
import Data.Monoid ( Monoid (..), (<>) )
import Data.String ( IsString (..) )
import Text.StringLike ( StringLike (..) )
import Control.Monad.RWS

-- | The abstract code echor monad. In @W a m@, @a@ is a string-like output
-- type, @m@ is any monad you want (as long as you can echo a function
-- @:: String -> m ()@ that processes a warning).
type W a m = RWST (String -> m ()) a WState m

data WState =
    WState
        { indentation :: Int
        , atLineStart :: Bool
        }
        deriving (Show, Eq, Read)

defWState :: WState
defWState = WState 0 True

-- | Run a 'W' action into its wrapped monad 'm', producing a string-like
-- output value in 'a'.
runW :: (StringLike a, IsString a, Monoid a, Monad m)
      => (String -> m ())
      -> W a m b
      -> m a
runW echoWarning a =
    snd <$> execRWST a echoWarning defWState

-- | Raise a warning in 'W'.
warn :: (Monoid a, Monad m) => String -> W a m ()
warn msg = do
    echoWarning <- ask
    lift $ echoWarning msg

-- | Increase indentation
indentMore :: (Monoid a, Monad m) => W a m ()
indentMore = modify $ \s -> s { indentation = succ (indentation s) }

-- | Decrease indentation
indentLess :: (Monoid a, Monad m) => W a m ()
indentLess = modify $ \s -> s { indentation = pred (indentation s) }

-- | @indented a@ wraps action @a@ to run in a context with one extra level of
-- intentation, similar to a bracket. @indented@ can be nested, each level
-- adding one level of indentation.
indented :: (Monoid a, Monad m) => W a m x -> W a m x
indented a = indentMore *> a <* indentLess

-- | Flavor of @indented@ that works on a transformer over 'W' rather than
-- 'W' directly.
indentedT :: (MonadTrans t, Monoid a, Monad m, Applicative (t (W a m)))
          => t (W a m) x
          -> t (W a m) x
indentedT a = lift indentMore *> a <* lift indentLess

echo :: (IsString a, Monoid a, Monad m) => a -> W a m ()
echo msg = do
    s <- get
    when (atLineStart s) $ tell (mconcat . replicate (indentation s) $ "  ")
    tell msg
    modify $ \s -> s { atLineStart = False }

endl :: (IsString a, Monoid a, Monad m) => W a m ()
endl = do
    tell "\n"
    modify $ \s -> s { atLineStart = True }
