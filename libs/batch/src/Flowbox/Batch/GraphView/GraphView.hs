---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.GraphView(
    module Flowbox.Data.Graph,
    GraphView,
    empty,
    
    isNotAlreadyConnected,

    fromGraph,
    toGraph
) where

import qualified Data.List                               as List
import           Data.Map                                  (Map)
import qualified Data.Map                                as Map
import           Data.Foldable                             (foldrM)

import           Flowbox.Prelude                           
import qualified Flowbox.Batch.Batch                     as Batch
import qualified Flowbox.Batch.GraphView.Defaults        as Defaults
import qualified Flowbox.Batch.GraphView.EdgeView        as EdgeView
import           Flowbox.Batch.GraphView.EdgeView          (EdgeView(EdgeView))
import           Flowbox.Batch.GraphView.PortDescriptor    (PortDescriptor)
import           Flowbox.Control.Error                     ()
import qualified Flowbox.Data.Graph                      as DG
import           Flowbox.Data.Graph                      hiding (Graph, Edge, empty, fromGraph, sp)
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import           Flowbox.Luna.Network.Graph.Graph          (Graph)
import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node(..))
import qualified Flowbox.Luna.Network.Graph.Port         as Port
import           Flowbox.Luna.Network.Graph.Port           (Port)
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import           Flowbox.Luna.Network.Attributes           (Attributes)
import qualified Flowbox.Luna.Network.Flags              as Flags



type GraphView = DG.Graph Node EdgeView


empty :: GraphView
empty = DG.empty


portMatches :: PortDescriptor -> LEdge EdgeView -> Bool
portMatches adstPort (_, _, connectedPort) = matches where
    connectedDstPort = EdgeView.dstPort connectedPort
    matches = List.isPrefixOf connectedDstPort adstPort
           || List.isPrefixOf adstPort connectedDstPort

isNotAlreadyConnected :: GraphView -> Node.ID -> PortDescriptor -> Bool
isNotAlreadyConnected graphview nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graphview nodeID)


------ Conversion to/from Graph --------------------------------------------------------

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


findMatchingTuple :: Node.ID -> Int -> Graph -> Maybe (Node.ID, Node.ID, Edge)
findMatchingTuple nodeID port graph = mt where
    mt = List.find matching (inn graph nodeID)

    matching :: (Node.ID, Node.ID, Edge) -> Bool
    matching (t, _, Edge _ (Port.Number existingTuplePort)) = case lab graph t of 
                              Just (Tuple {}) -> existingTuplePort == port
                              _               -> False
    matching _ = False

getOrCreateTuple :: Node.ID -> Int -> Graph -> Either String (Node.ID, Graph)
getOrCreateTuple nodeID port graph = case findMatchingTuple nodeID port graph of 
    Nothing                      -> do let [newTupleID] = Graph.newNodes 1 graph
                                           newTupleNode = Tuple Flags.empty generatedAttrs
                                           newGraphT    = Graph.insEdge (newTupleID, nodeID, Edge Port.All (Port.Number port))
                                                        $ Graph.insNode (newTupleID, newTupleNode) graph
                                       return (newTupleID, newGraphT)
    Just (existingTupleID, _, _) -> Right (existingTupleID, graph)


connectGTuples :: Node.ID -> Node.ID -> Port -> PortDescriptor -> Graph -> Either String Graph
connectGTuples srcNodeID dstNodeID srcPort dstPorts graph = case dstPorts of 
    []  -> return $ Graph.insEdge (srcNodeID, dstNodeID, Edge srcPort  Port.All      ) graph
    [p] -> return $ Graph.insEdge (srcNodeID, dstNodeID, Edge srcPort (Port.Number p)) graph
    _   -> do
              (tupleID, graphT) <- getOrCreateTuple dstNodeID (head dstPorts) graph
              connectGTuples srcNodeID tupleID srcPort (tail dstPorts) graphT


createSelect :: Int -> Node
createSelect p = Expr ("select" ++ show p) Flags.empty $ selectAttrs p


insertSelects :: PortDescriptor -> Graph -> (Node.ID, Node.ID, Graph)
insertSelects ports graph = case ports of 
    [p] -> (selectID, selectID, newGraph) where
           (newGraph, selectID) = Graph.insNewNode (createSelect p) graph
    _   -> (fSelectID, lSelectID, newGraph) where
           ( selectID, lSelectID, graph1) = insertSelects (tail ports) graph
           (graph2, fSelectID) = Graph.insNewNode (createSelect (head ports)) graph1
           newEdge             = Edge Port.All Port.All
           newGraph            = Graph.insEdge (fSelectID, selectID, newEdge) graph2


connectG :: (Node.ID, Node.ID, EdgeView) -> Graph -> Either String Graph
connectG (srcNodeID, dstNodeID, EdgeView srcPorts dstPorts) graph = case srcPorts of 
    []        -> connectGTuples srcNodeID    dstNodeID  Port.All             dstPorts graph
    [srcPort] -> connectGTuples srcNodeID    dstNodeID (Port.Number srcPort) dstPorts graph
    _         -> connectGTuples lastSelectID dstNodeID  Port.All             dstPorts newGraph where
                (firstSelectID, lastSelectID, graphWithSelects) = insertSelects (tail srcPorts) graph
                newEdge  = Edge (Port.Number (head srcPorts)) Port.All
                newGraph = Graph.insEdge (srcNodeID, firstSelectID, newEdge) graphWithSelects

addNodeDefaults :: GraphView -> (Node.ID, Node) -> Graph -> Either String Graph
addNodeDefaults graphview (nodeID, node) graph = do

    let 
        addNodeDefault :: (PortDescriptor, DefaultValue) -> Graph -> Either String Graph
        addNodeDefault (adstPort, defaultValue) g = do
            if isNotAlreadyConnected graphview nodeID adstPort
                then do let (newG1, defaultNodeID) = Graph.insNewNode (Default defaultValue generatedAttrs) g
                        connectG (defaultNodeID, nodeID, EdgeView [] adstPort) newG1
                else Right g

    let defaultsMap = Defaults.getDefaults node
    foldrM addNodeDefault graph $ Map.toList defaultsMap


addNodesDefaults :: GraphView -> Graph -> Either String Graph
addNodesDefaults graphview graphWithoutDefaults = 
    foldrM (addNodeDefaults graphview) graphWithoutDefaults (labNodes graphWithoutDefaults)


toGraph :: GraphView -> Either String Graph
toGraph graphview = do
    let graphWithoutEdges = removeEdges graphview
    graphWithoutDefaults <- foldrM (connectG) graphWithoutEdges $ labEdges graphview
    graph                <- addNodesDefaults graphview graphWithoutDefaults 
    return graph


graph2graphView :: Graph -> GraphView
graph2graphView graph = graphv where

    p2pd :: Port -> PortDescriptor
    p2pd port = case port of
        Port.All      -> []
        Port.Number p -> [p]

    edge2edgeView :: (Node.ID, Node.ID, Edge) -> (Node.ID, Node.ID, EdgeView)
    edge2edgeView (s, d, Edge sp dp) = 
        (s, d, EdgeView (p2pd sp) (p2pd dp))
    
    anodes  = labNodes graph
    aedges  = map edge2edgeView $ labEdges graph
    graphv = mkGraph anodes aedges


getBatchAttrs :: Node -> Maybe (Map String String)
getBatchAttrs node = Map.lookup Batch.attributeKey attrs where
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


delGenerated :: (Node.ID, Node) -> GraphView -> Either String GraphView
delGenerated (nodeID, node) graph = case isGenerated node of 
    False -> Right graph
    True  -> case node of 
        Tuple  {}  -> case (inn graph nodeID, out graph nodeID, suc graph nodeID) of
                        (inEdges, [(_, _, EdgeView _ op)], [adst]) -> Right $ delNode nodeID
                                                                          $ insEdges newEdges graph where 
                                                   newEdges = map (\(asrc, _, EdgeView a ip) -> (asrc, adst, EdgeView a (op++ip))) inEdges
                        _ -> Left "Batch attributes mismatch - incorrectly connected NTuple"
        Default {} -> Right $ delNode nodeID graph
        _          -> case (selectNo node, inn graph nodeID, out graph nodeID) of 
                        (Just num, [(asrc, _, EdgeView isrcPort _)], [(_, adst, EdgeView osrcPort odstPort)])
                            -> Right $ delNode nodeID 
                                     $ insEdge (asrc, adst, EdgeView (isrcPort ++ [num] ++ osrcPort) odstPort) graph
                        _   -> Left "Batch attributes mismatch - incorrectly connected Select"


fromGraph :: Graph -> Either String GraphView
fromGraph graph = do
    let graphC = graph2graphView graph
    foldrM (delGenerated) graphC $ labNodes graphC


