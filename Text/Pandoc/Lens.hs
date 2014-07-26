{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

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
      -- * Inlines
    , _Str
    , _Emph
    , _Strong
    , _Strikeout
    , _Superscript
    , _Subscript
    , _SmallCaps
    -- , _Quoted
    -- , _Cite
    , _Code
    , _Space
    , _LineBreak
    -- , _Math
    -- , _RawInline
    -- , _Link
    -- , _Image
    , _Note
    , _Span
      -- * Metadata values
    , meta
      -- * Attributes
    , HasAttr(..)
    ) where

import Control.Applicative
import Control.Lens
import Text.Pandoc.Definition
import Data.Map (Map)

-- | The body of a pandoc document
body :: Lens' Pandoc [Block]
body = lens (\(Pandoc _ b)->b) (\(Pandoc m _) b->Pandoc m b)

-- | A prism on a 'Plain' 'Block'
_Plain :: Prism' Block [Inline]
_Plain = prism' Plain f
  where
    f (Plain x) = Just x
    f _         = Nothing

-- | A prism on a paragraph 'Block'
_Para :: Prism' Block [Inline]
_Para = prism' Para f
  where
    f (Para x)  = Just x
    f _         = Nothing

-- | A prism on the text of a 'CodeBlock'
_CodeBlock :: Prism' Block String
_CodeBlock = prism' (CodeBlock nullAttr) f
  where
    f (CodeBlock _ x)    = Just x
    f _                  = Nothing

-- | A prism on a 'BlockQuote'
_BlockQuote :: Prism' Block [Block]
_BlockQuote = prism' BlockQuote f
  where
    f (BlockQuote x)     = Just x
    f _                  = Nothing

-- | A prism on the items of a bullet list 'Block'
_BulletList :: Prism' Block [[Block]]
_BulletList = prism' BulletList f
  where
    f (BulletList x)     = Just x
    f _                  = Nothing

-- | A prism on the items of a definition list 'Block'
_DefinitionList :: Prism' Block [([Inline], [[Block]])]
_DefinitionList = prism' DefinitionList f
  where
    f (DefinitionList x) = Just x
    f _                  = Nothing

-- | A prism on a 'HorizontalRule' 'Block'
_HorizontalRule :: Prism' Block ()
_HorizontalRule = prism' (const HorizontalRule) f
  where
    f HorizontalRule     = Just ()
    f _                  = Nothing

-- | A prism on a 'Null' 'Block'
_Null :: Prism' Block ()
_Null = prism' (const Null) f
  where
    f Null = Just ()
    f _    = Nothing

-- | A prism on a 'Str' 'Inline'
_Str :: Prism' Inline String
_Str = prism' Str f
  where
    f (Str s) = Just s
    f _       = Nothing

-- | A prism on an 'Emph' 'Inline'
_Emph :: Prism' Inline [Inline]
_Emph = prism' Emph f
  where
    f (Emph s) = Just s
    f _        = Nothing

-- | A prism on a 'Strong' 'Inline'
_Strong :: Prism' Inline [Inline]
_Strong = prism' Strong f
  where
    f (Strong s) = Just s
    f _          = Nothing

-- | A prism on a 'Strikeout' 'Inline'
_Strikeout :: Prism' Inline [Inline]
_Strikeout = prism' Strikeout f
  where
    f (Strikeout s) = Just s
    f _             = Nothing

-- | A prism on a 'Superscript' 'Inline'
_Superscript :: Prism' Inline [Inline]
_Superscript = prism' Superscript f
  where
    f (Superscript s) = Just s
    f _               = Nothing

-- | A prism on a 'Subscript' 'Inline'
_Subscript :: Prism' Inline [Inline]
_Subscript = prism' Subscript f
  where
    f (Subscript s) = Just s
    f _             = Nothing

-- | A prism on a 'SmallCaps' 'Inline'
_SmallCaps :: Prism' Inline [Inline]
_SmallCaps = prism' SmallCaps f
  where
    f (SmallCaps s) = Just s
    f _             = Nothing

-- | A prism on the body of a 'Code' 'Inline'
_Code :: Prism' Inline String
_Code = prism' (Code nullAttr) f
  where
    f (Code _ s) = Just s
    f _          = Nothing

-- | A prism on a 'Space' 'Inline'
_Space :: Prism' Inline ()
_Space = prism' (const Space) f
  where
    f Space = Just ()
    f _     = Nothing

-- | A prism on a 'LineBreak' 'Inline'
_LineBreak :: Prism' Inline ()
_LineBreak = prism' (const LineBreak) f
  where
    f LineBreak = Just ()
    f _         = Nothing

-- | A prism on a 'Note' 'Inline'
_Note :: Prism' Inline [Block]
_Note = prism' Note f
  where
    f (Note s) = Just s
    f _        = Nothing

-- | A prism on a 'Span' 'Inline'
_Span :: Prism' Inline [Inline]
_Span = prism' (Span nullAttr) f
  where
    f (Span _ s) = Just s
    f _          = Nothing

instance Plated Inline
instance Plated Block

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

instance HasAttr Inline where
  attributes f (Code a s) = fmap (\a'->Code a' s) (f a)
  attributes f (Span a s) = fmap (\a'->Span a' s) (f a)
  attributes _ x = pure x
