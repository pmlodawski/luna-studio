module Data.Graph.Model.Cluster where

import Prologue


type family ClusterOf a

class Clustered t where
    clusters :: Lens' t [ClusterOf t]



--module Luna.Syntax.Model.Graph.Cluster where

--import Prelude.Luna

--import Data.Graph.Referenced
--import Luna.Syntax.Model.Graph.Class

--import Data.Container
--import Data.Index
--import Data.Prop

--import           Data.IntSet (IntSet)
--import qualified Data.IntSet as IntSet

--import Data.Graph.Backend.Vector

---- === Definitions === --

----TODO[WD]: Refactor from Graph.hs


---- === Utils === --

--add :: Int -> Cluster -> Cluster
--add el = wrapped %~ IntSet.insert el

--remove :: Int -> Cluster -> Cluster
--remove el = wrapped %~ IntSet.delete el

--member :: Int -> Cluster -> Bool
--member el = IntSet.member el ∘ unwrap'


---- === Instances === --

---- Wrappers
--makeWrapped ''Cluster


---- Properties

----instance Getter (Ref Cluster) (VectorGraph n e) where getter ref     = index_ (ref ^. idx) ∘ view clusterGraph                    ; {-# INLINE getter #-}
----instance Setter (Ref Cluster) (VectorGraph n e) where setter ref val = clusterGraph %~ unchecked inplace insert_ (ref ^. idx) val ; {-# INLINE setter #-}
