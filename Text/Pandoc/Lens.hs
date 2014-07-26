{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Pandoc.Lens
    ( -- * Documents
      body
      -- * Blocks
    , _Plain
    , _Para
    , _CodeBlock
    , _BlockQuote
    , _BulletList
    , _DefinitionList
    , _HorizontalRule
    , _Null
      -- * Meta values
    , meta
      -- * Attributes
    , HasAttr(attributes)
    ) where

import Control.Applicative
import Control.Lens
import Text.Pandoc.Definition
import Data.Map (Map)

-- | The body of a pandoc document
body :: Lens' Pandoc [Block]
body = lens (\(Pandoc _ b)->b) (\(Pandoc m _) b->Pandoc m b)

-- | A prism on a plain block
_Plain :: Prism' Block [Inline]
_Plain = prism' Plain f
  where
    f (Plain x) = Just x
    f _         = Nothing

-- | A prism on a paragraph block
_Para :: Prism' Block [Inline]
_Para = prism' Para f
  where
    f (Para x)  = Just x
    f _         = Nothing

-- | A prism on the text of a code block
_CodeBlock :: Prism' Block String
_CodeBlock = prism' (CodeBlock nullAttr) f
  where
    f (CodeBlock _ x)    = Just x
    f _                  = Nothing

-- | A prism on a block quote
_BlockQuote :: Prism' Block [Block]
_BlockQuote = prism' BlockQuote f
  where
    f (BlockQuote x)     = Just x
    f _                  = Nothing

-- | A prism on the items of a bullet list block
_BulletList :: Prism' Block [[Block]]
_BulletList = prism' BulletList f
  where
    f (BulletList x)     = Just x
    f _                  = Nothing

-- | A prism on the items of a definition list
_DefinitionList :: Prism' Block [([Inline], [[Block]])]
_DefinitionList = prism' DefinitionList f
  where
    f (DefinitionList x) = Just x
    f _                  = Nothing

-- | A prism on a horizontal rule
_HorizontalRule :: Prism' Block ()
_HorizontalRule = prism' (const HorizontalRule) f
  where
    f HorizontalRule     = Just ()
    f _                  = Nothing

-- | A prism on a null block
_Null :: Prism' Block ()
_Null = prism' (const Null) f
  where
    f Null = Just ()
    f _    = Nothing

--makePrisms ''Inline
--makePrisms ''MetaValue

instance Wrapped Meta where
    type Unwrapped Meta = Map String MetaValue
    _Wrapped' = iso unMeta Meta

-- | A traversal focusing on a particular metadata value
meta :: String -> Traversal' Pandoc MetaValue
meta m = metaL . _Wrapped' . ix m
  where
    metaL :: Lens' Pandoc Meta
    metaL = lens (\(Pandoc m _)->m) (\(Pandoc _ a) m->Pandoc m a)

-- | An object that has attributes
class HasAttr a where
  -- | A traversal over the attributes of an object
  attributes :: Traversal' a Attr

instance HasAttr Block where
  attributes f (CodeBlock a s) = fmap (\a'->CodeBlock a' s) (f a)
  attributes f (Header n a s)  = fmap (\a'->Header n a' s) (f a)
  attributes f (Div a s)       = fmap (\a'->Div a' s) (f a)
  attributes _ x = pure x
