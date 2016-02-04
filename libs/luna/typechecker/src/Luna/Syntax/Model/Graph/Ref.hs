{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Ref where


import Prelude.Luna

import Data.Index
import Data.Construction
import Data.Layer


-- === Definitions === --

data Ptr i = Ptr i         deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Ref a = Ref (Ptr Int) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeWrapped ''Ptr
makeWrapped ''Ref

type family Target a

class HasPtr   a where ptr   :: Lens' a (Ptr (Index  a))
class HasRef   a where ref   :: Lens' a (Ref (Target a))



--class Monad m => Reader m a where read  :: Ref a -> m a
--class Monad m => Writer m a where write :: Ref a -> a -> m ()

-- === Utils === --

rawPtr :: HasPtr a => Lens' a (Index a)
rawPtr = ptr ∘ wrapped'




-- === Instances === --

-- Ptr primitive instances
type instance Index  (Ptr i) = i
instance      HasIdx (Ptr i) where idx = wrapped'
instance      HasPtr (Ptr i) where ptr = id

-- Ref primitive instances
type instance Unlayered     (Ref a) = a
type instance Deconstructed (Ref a) = a
type instance Target        (Ref a) = a
type instance Index         (Ref a) = Index (Unwrapped (Ref a))
instance      HasRef        (Ref a) where ref = id
instance      HasIdx        (Ref a) where idx = ptr ∘ idx
instance      HasPtr        (Ref a) where ptr = wrapped'

-- Conversions
instance Castable a a' => Castable (Ref a) (Ref a') where cast = rewrap ; {-# INLINE cast #-}

-- Construction
instance Constructor m (Ref ref) => LayerConstructor m (Ref ref) where
    constructLayer = construct ; {-# INLINE constructLayer #-}

