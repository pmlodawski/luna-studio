{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Luna.Syntax.Model.Graph.Class where

import Prologue hiding (Getter, Setter, read)

import Control.Monad.Event
import Data.Prop
import Data.Construction
import Data.Container
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)
import Data.IntSet              (IntSet)

import qualified Data.IntSet as IntSet


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



-------------------
-- === Graph === --
-------------------

data Graph node edge = Graph { _nodeGraph :: AutoVector node
                             , _edgeGraph :: AutoVector edge
                             , _clusters  :: AutoVector Cluster
                             } deriving (Show)

makeLenses  ''Graph


-- === Attributes === --

data ELEMENT    = ELEMENT    deriving (Show)
data CONNECTION = CONNECTION deriving (Show)


-- === Utils === --

nodes :: Lens' (Graph n e) [n]
nodes = nodeGraph ∘ asList

edges :: Lens' (Graph n e) [e]
edges = edgeGraph ∘ asList


-- === Instances === --

instance Default (Graph n e) where def = Graph (alloc 100) (alloc 100) (alloc 100)



