---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.GraphView(
    module Flowbox.Luna.Data.Graph,
    GraphView,
    empty,
    
    toGraph
) where

import qualified Data.Map                                 as Map
import           Flowbox.Batch.GraphView.EdgeView  			(EdgeView(..))
import qualified Flowbox.Luna.Data.Graph         		  as DG
import 			 Flowbox.Luna.Data.Graph         		  hiding(Graph, Edge, empty)
import qualified Flowbox.Luna.Network.Graph.Graph 	      as Graph
import           Flowbox.Luna.Network.Graph.Graph  		    (Graph)

import qualified Flowbox.Luna.Network.Graph.Edge  		  as Edge
import           Flowbox.Luna.Network.Graph.Edge    		(Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node  		  as Node
import           Flowbox.Luna.Network.Graph.Node    		(Node(..))
import qualified Flowbox.Luna.Network.Attributes          as Attributes
import           Flowbox.Luna.Network.Attributes            (Attributes)
import qualified Flowbox.Luna.Network.Flags               as Flags


type GraphView = DG.Graph Node EdgeView


empty :: GraphView
empty = DG.empty


nodesOnly :: DG.Graph a b -> DG.Graph a c
nodesOnly graph = graphv where
	nodes  = labNodes graph
	graphv = mkGraph nodes []


toGraph :: GraphView -> Graph
toGraph graphv = graph where
	graphN = nodesOnly graphv
	graph = foldr (connectG) graphN $ labEdges graphv


attributeKey :: String
attributeKey = "Batch-0.1"


generatedAttrs :: Attributes
generatedAttrs = Attributes.fromList [(attributeKey, Map.fromList [("Generated", "True")])]


connectG :: (Node.ID, Node.ID, EdgeView) -> Graph -> Graph
connectG (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph = case srcPort of 
    []       -> newGraph where
        tupleNode = NTuple Flags.empty generatedAttrs
        [tupleID] = Graph.newNodes 1 graph
        newGraph  = Graph.insEdge (srcNodeID, tupleID, Edge dstPort)
                  $ Graph.insEdge (tupleID, dstNodeID, Edge 0)
                  $ Graph.insNode (tupleID, tupleNode) graph
    srcPorts -> connectG (selectID, dstNodeID, EdgeView srcPortsTail dstPort) newGraph where
        srcPortsHead = head srcPorts
        srcPortsTail = tail srcPorts
        selectNode = Call ("select" ++ show srcPortsHead) Flags.empty generatedAttrs
        [selectID] = Graph.newNodes 1 graph
        newGraph = Graph.insEdge (srcNodeID, selectID, Edge 0)
                 $ Graph.insNode (selectID, selectNode) graph
