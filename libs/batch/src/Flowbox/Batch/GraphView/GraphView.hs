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
import qualified Flowbox.Batch.Batch              as Batch
import           Flowbox.Batch.GraphView.EdgeView   (EdgeView(..))
import qualified Flowbox.Luna.Data.Graph          as DG
import           Flowbox.Luna.Data.Graph          hiding (Graph, Edge, empty, fromGraph)
import qualified Flowbox.Luna.Network.Graph.Graph as Graph
import           Flowbox.Luna.Network.Graph.Graph   (Graph)

import           Flowbox.Luna.Network.Graph.Edge    (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node  as Node
import           Flowbox.Luna.Network.Graph.Node    (Node(..))
import qualified Flowbox.Luna.Network.Attributes  as Attributes
import           Flowbox.Luna.Network.Attributes    (Attributes)
import qualified Flowbox.Luna.Network.Flags       as Flags


type GraphView = DG.Graph Node EdgeView


empty :: GraphView
empty = DG.empty


isGeneratedKey :: String
isGeneratedKey = "GraphView-generated"


selectKey :: String
selectKey = "GraphView-select"


trueVal :: String
trueVal = "True"


generatedAttrs :: Attributes
generatedAttrs = Attributes.fromList [(Batch.attributeKey, Map.fromList [(isGeneratedKey, trueVal)])]


selectAttrs :: Int -> Attributes
selectAttrs num = Attributes.fromList [(Batch.attributeKey, Map.fromList [(isGeneratedKey, trueVal)
                                                                   ,(selectKey, show num)])]

removeEdges :: DG.Graph a b -> DG.Graph a c
removeEdges graph = mkGraph (labNodes graph) []


connectG :: (Node.ID, Node.ID, EdgeView) -> Graph -> Graph
connectG (srcNodeID, dstNodeID, EdgeView srcPorts dstPorts) graph = case srcPorts of 
    [] -> case dstPorts of 
        []         -> Graph.insEdge (srcNodeID, dstNodeID, Edge 0       ) graph
        [adstPort] -> Graph.insEdge (srcNodeID, tupleID  , Edge adstPort) graphT where
            (tupleID, graphT) = case prel graph dstNodeID of 
                []                             -> (newTupleID, newGraphT) where
                    [newTupleID] = Graph.newNodes 1 graph
                    newTupleNode = NTuple Flags.empty generatedAttrs
                    newGraphT    = Graph.insEdge (tupleID, dstNodeID, Edge 0)
                                 $ Graph.insNode (tupleID, newTupleNode) graph
                [(existingTupleID, NTuple {})] -> (existingTupleID, graph)
                _                      -> error "Connect nodes failed"
        _         -> error "dst port descriptors cannot have lenght greater than 1."
    _ -> connectG (selectID, dstNodeID, EdgeView srcPortsTail dstPorts) newGraph where
        srcPortsHead = head srcPorts
        srcPortsTail = tail srcPorts
        selectNode = Call ("select" ++ show srcPortsHead) Flags.empty $ selectAttrs srcPortsHead
        [selectID] = Graph.newNodes 1 graph
        newGraph = Graph.insEdge (srcNodeID, selectID, Edge 0)
                 $ Graph.insNode (selectID, selectNode) graph


toGraph :: GraphView -> Graph
toGraph graphv = graph where
    graphN = removeEdges graphv
    graph = foldr (connectG) graphN $ labEdges graphv


graph2graphView :: Graph -> GraphView
graph2graphView graph = graphv where

    edge2edgeView :: (Node.ID, Node.ID, Edge) -> (Node.ID, Node.ID, EdgeView)
    edge2edgeView (s, d, Edge adst) = case lab graph d of
        Just (NTuple {}) -> (s, d, EdgeView [] [adst])
        _                -> (s, d, EdgeView [] [])
    
    anodes  = labNodes graph
    aedges  = map edge2edgeView $ labEdges graph
    graphv = mkGraph anodes aedges


fromGraph :: Graph -> GraphView
fromGraph graph = graphv where
    graphC = graph2graphView graph
    graphv = foldr (delGenerated) graphC $ labNodes graphC


getBatchAttrs :: Node -> Maybe (Map String String)
getBatchAttrs node = case node of
    Default {} -> Nothing
    _          -> Map.lookup Batch.attributeKey attrs where
                  attrs = Node.attributes node
                  

isGenerated :: Node -> Bool
isGenerated node = case getBatchAttrs node of
    Nothing         -> False
    Just batchAttrs -> case Map.lookup isGeneratedKey batchAttrs of 
        Just "True" -> True
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
            (inEdges, [adst])
                -> delNode nodeID 
                $ insEdges newEdges graph where 
                    newEdges = map (\(asrc, _, ev) -> (asrc, adst, ev)) inEdges
            _ -> error "Batch attributes mismatch - incorrectly connected NTuple"
        _         -> case (selectNo node, inn graph nodeID, out graph nodeID) of 
            (Just num, [(asrc, _, EdgeView isrcPort _)], [(_, adst, EdgeView osrcPort odstPort)])
                -> delNode nodeID 
                 $ insEdge (asrc, adst, EdgeView (isrcPort ++ [num] ++ osrcPort) odstPort) graph
            _   -> error "Batch attributes mismatch - incorrectly connected Select"




