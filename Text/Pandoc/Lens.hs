{-# LANGUAGE RankNTypes #-}

module Text.Pandoc.Lens where
                
import Control.Applicative
import Control.Lens
import Text.Pandoc.Definition
import Data.Map (Map)

body :: Lens' Pandoc [Block]
body = lens (\(Pandoc _ b)->b) (\(Pandoc m _) b->Pandoc m b)

_Plain :: Prism' Block [Inline]
_Plain = prism' Plain f
  where
    f (Plain x) = Just x
    f _         = Nothing

_Para :: Prism' Block [Inline]
_Para = prism' Para f
  where
    f (Para x)  = Just x
    f _         = Nothing

_CodeBlock :: Prism' Block String
_CodeBlock = prism' (CodeBlock nullAttr) f
  where
    f (CodeBlock _ x)    = Just x
    f _                  = Nothing

_BlockQuote :: Prism' Block [Block]
_BlockQuote = prism' BlockQuote f
  where
    f (BlockQuote x)     = Just x
    f _                  = Nothing

_BulletList :: Prism' Block [[Block]]
_BulletList = prism' BulletList f
  where
    f (BulletList x)     = Just x
    f _                  = Nothing
      
_DefinitionList :: Prism' Block [([Inline], [[Block]])]
_DefinitionList = prism' DefinitionList f
  where
    f (DefinitionList x) = Just x
    f _                  = Nothing

_HorizontalRule :: Prism' Block ()
_HorizontalRule = prism' (const HorizontalRule) f
  where
    f HorizontalRule     = Just ()
    f _                  = Nothing

_Null :: Prism' Block ()
_Null = prism' (const Null) f
  where
    f Null = Just ()
    f _    = Nothing

--makePrisms ''Inline
--makePrisms ''MetaValue

--meta :: String -> Prism' Pandoc MetaValue
meta m = metaL . unwrap . ix m
  where
    unwrap :: Iso' Meta (Map String MetaValue)
    unwrap = iso unMeta Meta
    metaL :: Lens' Pandoc Meta
    metaL = lens (\(Pandoc m _)->m) (\(Pandoc _ a) m->Pandoc m a)
  
class HasAttr a where
  attributes :: Traversal' a Attr
  
instance HasAttr Block where
  attributes f (CodeBlock a s) = fmap (\a'->CodeBlock a' s) (f a)
  attributes f (Header n a s)  = fmap (\a'->Header n a' s) (f a)
  attributes f (Div a s)       = fmap (\a'->Div a' s) (f a)
  attributes _ x = pure x
