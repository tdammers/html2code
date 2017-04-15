{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}
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
import Data.Maybe (catMaybes)
import Data.Char (toLower)
import Data.List (intercalate)
import Safe (readMay)

halogen :: (StringLike a, IsString a, Monoid a, Monad m)
        => Language a m
halogen = Language
    { lBeginTag = \name attrs -> do
        echo $ "HH." <> fromString (qualifiedName name)
        endl
    , lEndTag = \name attrs ->
        pure ()
    , lText = \str -> do
        echo $ "HH.text " <> (quoteStr (fromString str))
    , lBeginAttribs =
        echo "[ "
    , lSepAttribs =
        echo ", "
    , lEndAttribs =
        echo "]" >> endl
    , lAttrib = halogenAttrib
    , lAttribVisible = const True
        
    , lBeginChildren =
        echo "[ "
    , lSepChildren =
        echo ", "
    , lEndChildren =
        echo "]"
    }

write :: (StringLike a, IsString a, Monoid a, Monad m)
      => (String -> m ())
      -> [XmlTree]
      -> m a
write = G.write halogen

quoteStr :: (StringLike a, IsString a) => a -> a
quoteStr = fromString . show . toString

halogenAttrib :: (StringLike a, IsString a, Monoid a, Monad m)
              => QName -> a -> W a m ()
halogenAttrib name value = do
    (name', value') <-
        either fail pure $ translateAttribute (name, toString value)
    echo $ fromString name'
    echo $ " "
    echo $ fromString value'

translateAttribute :: (QName, String) -> Either String (String, String)
translateAttribute (name, val) = do
    case map toLower . qualifiedName $ name of
        "class" -> do
            let attrsStr = intercalate ", " $
                    [ "HC.ClassName " <> show className
                    | className <- words val
                    ]
            pure
                ( "HP.classes"
                , "[" <> attrsStr <> "]"
                )
        "alt" -> autoStr
        "charset" -> autoStr
        "cols" -> autoInt
        "rows" -> autoInt
        "colspan" -> named "HP.colSpan" <$> autoInt
        "rowspan" -> named "HP.rowSpan" <$> autoInt
        "for" -> named "htmlFor" <$> autoStr
        -- "width" -> autoPixel -- TODO
        -- "height" -> autoPixel -- TODO
        "href" -> autoStr
        "id" -> underscored <$> autoStr
        "name" -> autoStr
        "rel" -> autoStr
        "src" -> autoStr
        "target" -> autoStr
        "title" -> autoStr
        -- "method" -> autoFormMethod -- TODO
        "action" -> autoStr
        "novalidate" -> named "HP.noValidate" <$> autoBool
        -- "type" -> underscored <$> autoType -- TODO
        "value" -> autoStr
        "step" -> autoStr
        "disabled" -> autoBool
        "required" -> autoBool
        "readonly" -> named "HP.readOnly" <$> autoBool
        "spellcheck" -> named "HP.spellCheck" <$> autoBool
        "checked" -> autoBool
        "selected" -> autoBool
        "placeholder" -> autoStr
        "autocomplete" -> autoBool
        "multiple" -> autoBool
        "autoplay" -> autoBool
        "controls" -> autoBool
        "loop" -> autoBool
        "muted" -> autoBool
        "poster" -> autoStr
        -- "preload" -> autoPreload -- TODO
        "draggable" -> autoBool
        "tabindex" -> named "HP.tabIndex" <$> autoInt

        _ -> pure ( "HP.prop (HC.PropName " <> show name <> ")"
                  , show val
                  )
    where
      auto :: (String -> Either String String) -> Either String (String, String)
      auto convert = 
          (("HP." <> qualifiedName name),) <$> convert val

      autoStr :: Either String (String, String)
      autoStr = auto (pure . show)

      autoInt :: Either String (String, String)
      autoInt = auto $ \v -> do
          show <$> maybe
              (Left $ "Invalid integer " <> v)
              Right
              (readMay v :: Maybe Int)

      autoBool :: Either String (String, String)
      autoBool = auto $ \case
          "" -> pure "false"
          _ -> pure "true"

      named :: String -> (String, a) -> (String, a)
      named n (_, x) = (n, x)

      underscored :: (String, a) -> (String, a)
      underscored (s, x) = (s <> "_", x)

