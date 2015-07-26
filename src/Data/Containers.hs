{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Containers where

import Prelude

import           Control.Lens

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Data.Maybe (fromJust)


-- === ElementOf ===
type family ElementOf a

-- instances
type instance ElementOf [a] = a
type instance ElementOf (Map k v) = v
type instance ElementOf (Vector a) = a
type instance ElementOf (IntMap a) = a


-- === HasSize ===

class HasSize a where
    size :: a -> Int


-- instances

instance HasSize [a] where
    size = length

instance HasSize (Vector a) where
    size = Vector.length

instance HasSize (Map k v) where
    size = Map.size

instance HasSize (IntMap v) where
    size = IntMap.size


-- === Appendable ===

class Appendable a where
    append :: a -> ElementOf a -> a

class Prependable a where
    prepend :: ElementOf a -> a -> a

instance Appendable [a] where
    append as a = as ++ [a]

instance Prependable [a] where
    prepend = (:)

instance Appendable (Vector a) where
    append = Vector.snoc

instance Prependable (Vector a) where
    prepend = Vector.cons



class Appendable' m where
    append' :: m a -> a -> m a

class Prependable' m where
    prepend' :: a -> m a -> m a


instance Appendable' Vector where
    append' = Vector.snoc

instance Prependable' Vector where
    prepend' = Vector.cons