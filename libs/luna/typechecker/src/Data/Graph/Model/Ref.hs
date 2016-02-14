{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Ref where

import Prelude.Luna

import           Data.Construction
import           Data.Direction
import           Data.Index
import           Data.Layer
import           Data.Prop


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

