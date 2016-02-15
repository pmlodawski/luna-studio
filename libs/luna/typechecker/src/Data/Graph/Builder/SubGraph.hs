module Data.Graph.Builder.SubGraph where

import Prologue
import Data.Graph
import Data.Index
import Data.Construction
import Data.Graph.Backend.VectorGraph

import qualified Data.Graph.Backend.VectorGraph.SubGraph as SubGraph
import qualified Data.Graph.Builder.Class                as Graph
import qualified Data.Graph.Builder.Ref                  as Ref

subgraph :: Constructor m (Ref Cluster SubGraph) => m (Ref Cluster SubGraph)
subgraph = constructLayer $ SubGraph mempty

includes :: (Graph.MonadBuilder t m, Referred Cluster SubGraph t) => Ref Cluster SubGraph -> Ref Node a -> m Bool
includes cluster el = SubGraph.member (el ^. idx) <$> Ref.read cluster

include :: (Referred Cluster SubGraph t, Graph.MonadBuilder t m) => Ref Node a -> Ref Cluster SubGraph -> m ()
include el cluster = Ref.with cluster $ SubGraph.add   (el ^. idx)

exclude :: (Referred Cluster SubGraph t, Graph.MonadBuilder t m) => Ref Node a -> Ref Cluster SubGraph -> m ()
exclude el cluster = Ref.with cluster $ SubGraph.remove (el ^. idx)
