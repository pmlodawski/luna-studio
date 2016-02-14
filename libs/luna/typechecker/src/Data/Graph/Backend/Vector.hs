{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Backend.Vector where


import Prologue

import Data.Prop
import Data.Container
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)
import Data.IntSet              (IntSet)

import qualified Data.IntSet as IntSet

import Data.Graph.Model


----------------------------
-- === Data container === --
----------------------------

newtype AutoVector a = AutoVector (Auto Exponential (Vector a)) deriving (Show, Default)


-- === Instances === --

-- Wrappers
makeWrapped ''AutoVector
type instance Unlayered (AutoVector a) = Unwrapped (AutoVector a)
instance      Layered   (AutoVector a)

-- List conversions
type instance Item (AutoVector a) = Item (Unwrapped (AutoVector a))
deriving instance ToList   (AutoVector a)
deriving instance FromList (AutoVector a)

-- Containers
type instance Container (AutoVector a) = Container (Unwrapped (AutoVector a))
instance Monad m => HasContainerM m (AutoVector a) where
    viewContainerM = viewContainerM . unwrap ; {-# INLINE viewContainerM #-}
    setContainerM  = wrapped . setContainerM ; {-# INLINE setContainerM  #-}

instance Monad m => IsContainerM  m (AutoVector a) where
    fromContainerM = fmap AutoVector . fromContainerM ; {-# INLINE fromContainerM #-}



---------------------
-- === Cluster === --
---------------------

newtype Cluster = Cluster IntSet deriving (Show)



-------------------------
-- === VectorGraph === --
-------------------------

data VectorGraph node edge = VectorGraph { _nodeGraph    :: AutoVector node
                                         , _edgeGraph    :: AutoVector edge
                                         , _clusterGraph :: AutoVector Cluster
                                         } deriving (Show)

makeLenses  ''VectorGraph




-- === Utils === --

nodes :: Lens' (VectorGraph n e) [n]
nodes = nodeGraph ∘ asList

edges :: Lens' (VectorGraph n e) [e]
edges = edgeGraph ∘ asList


-- === Instances === --

instance Default (VectorGraph n e) where def = VectorGraph (alloc 100) (alloc 100) (alloc 100)



type Graph = VectorGraph
