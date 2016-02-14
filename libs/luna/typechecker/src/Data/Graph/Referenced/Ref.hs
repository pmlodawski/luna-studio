{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Referenced.Ref where

import Prelude.Luna

import Data.Construction
import Data.Direction
import Data.Index
import Data.Layer
import Data.Prop
import Data.Graph.Model

-----------------
-- === Ref === --
-----------------

-- === Definitions === --

newtype Ref a = Ref Int deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

class HasRef a t where ref :: Ref a -> Lens' t a



-- === Instances === --

-- Wrappers
makeWrapped ''Ref

-- Ref primitive instances
type instance Uncovered     (Ref a) = Uncovered a
type instance Unlayered     (Ref a) = a
type instance Deconstructed (Ref a) = a

-- Index
type instance Index  (Ref a) = Int
instance      HasIdx (Ref a) where idx = wrapped' ; {-# INLINE idx #-}

-- Conversions
instance Castable a a' => Castable (Ref a) (Ref a') where cast = rewrap ; {-# INLINE cast #-}

-- Construction
instance Constructor m (Ref ref) => LayerConstructor m (Ref ref) where
    constructLayer = construct ; {-# INLINE constructLayer #-}


-- Hetero reference handling

-- | When referencing the Hetero graph, we query the underlying one for its native node and edge representations
--   by using `NodeOf` and `EdgeOf` families respectively.

instance (HasRef (Node n') a, BiCastable n n', n' ~ NodeOf a)
      => HasRef (Node n) (Hetero a) where ref r = wrapped' ∘ ref (cast r :: Ref (Node n')) ∘ casted ; {-# INLINE ref #-}
instance HasRef (Node I) (Hetero a) where ref   = impossible
instance HasRef (Node n) (Hetero I) where ref   = impossible

instance (HasRef (Edge e') a, BiCastable e e', e' ~ EdgeOf a)
      => HasRef (Edge e) (Hetero a) where ref r = wrapped' ∘ ref (cast r :: Ref (Edge e')) ∘ casted ; {-# INLINE ref #-}
instance HasRef (Edge I) (Hetero a) where ref   = impossible
instance HasRef (Edge e) (Hetero I) where ref   = impossible
