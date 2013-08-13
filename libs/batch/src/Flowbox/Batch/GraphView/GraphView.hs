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

    fromGraph,
    toGraph
) where

import           Data.Map                           (Map)
import qualified Data.Map                         as Map
import           Flowbox.Batch.GraphView.EdgeView   (EdgeView(..))
import qualified Flowbox.Luna.Data.Graph          as DG
import           Flowbox.Luna.Data.Graph                  hiding(Graph, Edge, empty, fromGraph)
import qualified Flowbox.Luna.Network.Graph.Graph as Graph
import           Flowbox.Luna.Network.Graph.Graph   (Graph)

import qualified Flowbox.Luna.Network.Graph.Edge  as Edge
import           Flowbox.Luna.Network.Graph.Edge    (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node  as Node
import           Flowbox.Luna.Network.Graph.Node    (Node(..))
import qualified Flowbox.Luna.Network.Attributes  as Attributes
import           Flowbox.Luna.Network.Attributes    (Attributes)
import qualified Flowbox.Luna.Network.Flags       as Flags


type GraphView = DG.Graph Node EdgeView


empty :: GraphView
empty = DG.empty


attributeKey :: String
attributeKey = "Batch-0.1"

isGeneratedKey :: String
isGeneratedKey = "GraphView-generated"

selectKey :: String
selectKey = "GraphView-select"

trueVal :: String
trueVal = "True"

generatedAttrs :: Attributes
generatedAttrs = Attributes.fromList [(attributeKey, Map.fromList [(isGeneratedKey, trueVal)])]

selectAttrs :: Int -> Attributes
selectAttrs num = Attributes.fromList [(attributeKey, Map.fromList [(isGeneratedKey, trueVal)
                                                                   ,(selectKey, show num)])]


nodesOnly :: DG.Graph a b -> DG.Graph a c
nodesOnly graph = graphv where
    nodes  = labNodes graph
    graphv = mkGraph nodes []


connectG :: (Node.ID, Node.ID, EdgeView) -> Graph -> Graph
connectG (srcNodeID, dstNodeID, EdgeView srcPort dstPort) graph = case srcPort of 
    []       -> newGraph where

        (tupleID, graphT) = case prel graph dstNodeID of 
            []                   -> (tupleID, graphT) where
                [tupleID] = Graph.newNodes 1 graph
                tupleNode = NTuple Flags.empty generatedAttrs
                graphT    = Graph.insEdge (tupleID, dstNodeID, Edge 0)
                          $ Graph.insNode (tupleID, tupleNode) graph
            [(tupleID, NTuple {})] -> (tupleID, graph)
            _                      -> error "Connect nodes failed"
        
        newGraph  = Graph.insEdge (srcNodeID, tupleID, Edge dstPort) graphT
                  
    srcPorts -> connectG (selectID, dstNodeID, EdgeView srcPortsTail dstPort) newGraph where
        srcPortsHead = head srcPorts
        srcPortsTail = tail srcPorts
        selectNode = Call ("select" ++ show srcPortsHead) Flags.empty $ selectAttrs srcPortsHead
        [selectID] = Graph.newNodes 1 graph
        newGraph = Graph.insEdge (srcNodeID, selectID, Edge 0)
                 $ Graph.insNode (selectID, selectNode) graph


toGraph :: GraphView -> Graph
toGraph graphv = graph where
    graphN = nodesOnly graphv
    graph = foldr (connectG) graphN $ labEdges graphv



prepare :: Graph -> GraphView
prepare graph = graphv where
    nodes  = labNodes graph
    edges  = map (\(s, d, Edge dst) -> (s, d, EdgeView [] dst)) $ labEdges graph
    graphv = mkGraph nodes edges


fromGraph :: Graph -> GraphView
fromGraph graph = graphv where
    graphC = prepare graph
    graphv = foldr (delGenerated) graphC $ labNodes graphC


getBatchAttrs :: Node -> Maybe (Map String String)
getBatchAttrs node = case node of
    Default {} -> Nothing
    _          -> Map.lookup attributeKey attrs where
                  attrs = Node.attributes node
                  

isGenerated :: Node -> Bool
isGenerated node = case getBatchAttrs node of
    Nothing         -> False
    Just batchAttrs -> case Map.lookup isGeneratedKey batchAttrs of 
        Just trueVal -> True
        _           -> False


selectNo :: Node -> Maybe Int
selectNo node = do 
    batchAttrs <- getBatchAttrs node
    num        <- Map.lookup selectKey batchAttrs
    return $ read num


delGenerated :: (Node.ID, Node) -> GraphView -> GraphView
delGenerated (nodeID, node) graph = case isGenerated node of 
    False -> graph
    True  -> case node of 
        NTuple {} -> case (inn graph nodeID, suc graph nodeID) of
            (inEdges, [dst])
                -> delNode nodeID 
                $ insEdges newEdges graph where 
                    newEdges = map (\(src, _, ev) -> (src, dst, ev)) inEdges
            _ -> error "Batch attributes mismatch - incorrectly connected NTuple"
        _         -> case (selectNo node, inn graph nodeID, out graph nodeID) of 
            (Just num, [(src, _, EdgeView isrcPort _)], [(_, dst, EdgeView osrcPort odstPort)])
                -> delNode nodeID 
                 $ insEdge (src, dst, EdgeView (isrcPort ++ [num] ++ osrcPort) odstPort) graph
            _   -> error "Batch attributes mismatch - incorrectly connected Select"




