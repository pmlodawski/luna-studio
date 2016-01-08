module Utils.Graph.Components where

import Utils.PreludePlus
import Data.Graph.Inductive.PatriciaTree (UGr)
import Data.Graph.Inductive.Query.DFS    (dff', components)
import Data.Graph.Inductive              (subgraph, nodes, edges, mkUGraph)
import Empire.API.Data.Node (NodeId)

type Component = ([NodeId], [(NodeId, NodeId)])

splitComponents :: [NodeId] -> [(NodeId, NodeId)] -> [Component]
splitComponents = (fmap unGraph) .: (makeComponents .: makeGraph)

makeGraph :: [NodeId] -> [(NodeId, NodeId)] -> UGr
makeGraph = mkUGraph

makeComponents :: UGr -> [UGr]
makeComponents graph = (flip subgraph graph) <$> components graph

unGraph :: UGr -> ([NodeId], [(NodeId, NodeId)])
unGraph graph = (nodes graph, edges graph)
