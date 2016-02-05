{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Ref where

import Prelude.Luna

import Data.Prop
import qualified Data.Attr as Attr
import           Data.Attr (HasAttr, Attr, attr)
import Data.Index
import Data.Construction
import Data.Layer
import Luna.Syntax.Model.Graph.Class
import Data.Direction


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

-- Ptr primitive instances
type instance Index  (Ptr i) = i
instance      HasIdx (Ptr i) where idx = wrapped'
instance      HasPtr (Ptr i) where ptr = id



-----------------
-- === Ref === --
-----------------

-- === Definitions === --

data Ref a = Ref (Ptr Int) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeWrapped ''Ptr
makeWrapped ''Ref


-- === Instances === --

-- Ref primitive instances
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
