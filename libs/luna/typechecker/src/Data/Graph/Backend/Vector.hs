{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Graph.Backend.Vector where


import Prologue                 hiding (Getter, Setter)

import Data.Prop
import Data.Container           hiding (impossible)
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)
import Data.IntSet              (IntSet)

import qualified Data.IntSet as IntSet

import Data.Graph.Referenced


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

data VectorGraph node edge = VectorGraph { _nodeGraph    :: !(AutoVector node)
                                         , _edgeGraph    :: !(AutoVector edge)
                                         , _clusterGraph :: !(AutoVector Cluster)
                                         } deriving (Show)

makeLenses  ''VectorGraph


-- === Instances === --

-- Construction

type instance Prop Node (VectorGraph n e) = n
type instance Prop Edge (VectorGraph n e) = e
instance Default (VectorGraph n e) where def = VectorGraph (alloc 100) (alloc 100) (alloc 100)

-- Graph class

--type instance Item (VectorGraph)
--class Graph g where
--    nodes :: Lens' g [Node (Item g)]
--    edges :: Lens' g [Link (Item g)]

--instance Referrenced g where
--    nodeRefs :: Lens' g [Ref $ Node (Item g)]
--    edgeRefs :: Lens' g [Ref $ Link (Item g)]

--type family Generalized a
--class Generalize a where
--    generalize :: a -> Generalized a

----type instance Generalized

--type instance Generalized (Ref a) = Ref  (Generalized a)
--instance      Generalize  (Ref a) where generalize = rewrap

--type instance            Generalized (Node a) = Node (Generalized a)
--instance Generalize a => Generalize  (Node a) where generalize = wrapped %~ generalize


--type instance            Generalized (Edge a) = Edge (Generalized a)
--instance Generalize a => Generalize  (Edge a) where generalize = wrapped %~ generalize




-- References handling

instance r ~ n => Referred Node r (VectorGraph n e) where
    focus r = lens getter setter where
        getter t     = index_ (r ^. idx) $ t ^. nodeGraph                        ; {-# INLINE getter #-}
        setter t val = t & nodeGraph %~ unchecked inplace insert_ (r ^. idx) val ; {-# INLINE setter #-}
    {-# INLINE focus #-}

instance r ~ e => Referred Edge r (VectorGraph n e) where
    focus r = lens getter setter where
        getter t     = index_ (r ^. idx) $ t ^. edgeGraph                        ; {-# INLINE getter #-}
        setter t val = t & edgeGraph %~ unchecked inplace insert_ (r ^. idx) val ; {-# INLINE setter #-}
    {-# INLINE focus #-}


--instance Castable n node => Getter (Ref (Node node)) (VectorGraph n e) where getter ref     = Node ∘ cast ∘ index_ (ref ^. idx) ∘ view nodeGraph                       ; {-# INLINE getter #-}
--instance Castable node n => Setter (Ref (Node node)) (VectorGraph n e) where setter ref val = nodeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast $ unwrap' val) ; {-# INLINE setter #-}

--instance Castable e (Edge src tgt) => Getter (Ref (Edge src tgt)) (VectorGraph n e) where getter ref     = cast ∘ index_ (ref ^. idx) ∘ view edgeGraph                    ; {-# INLINE getter #-}
--instance Castable (Edge src tgt) e => Setter (Ref (Edge src tgt)) (VectorGraph n e) where setter ref val = edgeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast val) ; {-# INLINE setter #-}




--type instance NodeOf (VectorGraph n e) = n
--type instance EdgeOf (VectorGraph n e) = e
--instance      Graph  (VectorGraph n e) where
--    nodes = nodeGraph ∘ asList ; {-# INLINE nodes #-}
--    edges = edgeGraph ∘ asList ; {-# INLINE edges #-}

--type instance ClusterOf (VectorGraph n e) = Cluster
--instance      Clustered (VectorGraph n e) where
--    clusters = clusterGraph ∘ asList ; {-# INLINE clusters #-}

