{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Ref where

import Prelude.Luna

import           Data.Attr                     (HasAttr, Attr, attr)
import qualified Data.Attr                     as Attr
import           Data.Construction
import           Data.Direction
import           Data.Index
import           Data.Layer
import           Data.Prop
import           Luna.Syntax.Model.Graph.Class


-----------------
-- === Ptr === --
-----------------

-- === Definitions === --

data Ptr i = Ptr i deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

class HasPtr a where ptr :: Lens' a (Ptr (Index a))


-- === Utils === --

rawPtr :: HasPtr a => Lens' a (Index a)
rawPtr = ptr ∘ wrapped'

-- === Instances === --

-- Wrappers
makeWrapped ''Ptr
type instance Unlayered (Ptr i) = Unwrapped (Ptr i)
instance      Layered   (Ptr i)

-- Primitive instances
type instance Index  (Ptr i) = i
instance      HasIdx (Ptr i) where idx = wrapped'
instance      HasPtr (Ptr i) where ptr = id



-----------------
-- === Ref === --
-----------------

-- === Definitions === --

data Ref a = Ref (Ptr Int) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)


-- === Instances === --

-- Wrappers
makeWrapped ''Ref

-- Ref primitive instances
type instance Uncovered     (Ref a) = Uncovered (Unlayered (Ref a))
type instance Unlayered     (Ref a) = a
type instance Deconstructed (Ref a) = a
type instance Index         (Ref a) = Index (Unwrapped (Ref a))
instance      HasIdx        (Ref a) where idx = ptr ∘ idx ; {-# INLINE idx #-}
instance      HasPtr        (Ref a) where ptr = wrapped'  ; {-# INLINE ptr #-}

-- Conversions
instance Castable a a' => Castable (Ref a) (Ref a') where cast = rewrap ; {-# INLINE cast #-}

-- Construction
instance Constructor m (Ref ref) => LayerConstructor m (Ref ref) where
    constructLayer = construct ; {-# INLINE constructLayer #-}

-- Ref accessors
type instance Prop (Ref a) (Graph n e) = a
