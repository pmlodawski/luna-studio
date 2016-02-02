{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Data.Attribute where

import Prelude
import Control.Lens
import Data.Typeable


-- | The pragma is used to in-place expand the Lens definition, because GHC doesn't allow
--   for nested type synonyms for universal types
#define LENS_(a,b) ((b) -> f (b)) -> a -> f (a)


-- === Attribute accessors === --
-- | The class `HasAttrib a t` indicates that the type `t` is always provided with attribute `a`.
--   On the other hand, `MayHaveAttr a t` describes structures which could, but don't have to provide such attribute.


type family Attr a t
class MayHaveAttr a t => HasAttr     a t where attr      ::              a ->        Lens' t (Attr a t)
class                    MayHaveAttr a t where checkAttr :: Functor f => a -> Maybe (LENS_(t, Attr a t))
                                               default checkAttr :: (HasAttr a t, Functor f) => a -> Maybe (LENS_(t, Attr a t))
                                               checkAttr = Just <$> attr
                                               {-# INLINE checkAttr #-}

