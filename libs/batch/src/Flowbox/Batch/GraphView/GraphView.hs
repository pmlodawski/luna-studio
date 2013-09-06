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
    
    isNotAlreadyConnected,

    fromGraph,
    toGraph
) where

import qualified Data.List                               as List
import           Data.Map                                  (Map)
import qualified Data.Map                                as Map
import           Data.Foldable                             (foldrM)

import qualified Flowbox.Batch.Batch                     as Batch
import qualified Flowbox.Batch.GraphView.Defaults        as Defaults
import qualified Flowbox.Batch.GraphView.EdgeView        as EdgeView
import           Flowbox.Batch.GraphView.EdgeView          (EdgeView(..))
import           Flowbox.Batch.GraphView.PortDescriptor    (PortDescriptor)
import qualified Flowbox.Luna.Data.Graph                 as DG
import           Flowbox.Luna.Data.Graph                 hiding (Graph, Edge, empty, fromGraph)
import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import           Flowbox.Luna.Network.Graph.Graph          (Graph)
import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node(..))
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import           Flowbox.Luna.Network.Attributes           (Attributes)
import qualified Flowbox.Luna.Network.Flags              as Flags
import           Flowbox.Control.Error                     ()



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
findMatchingTuple nodeID p graph = mt where
    mt = List.find matching (inn graph nodeID)
    matching (t, _, Edge existingTuplePort) = case lab graph t of 
                                                    Just (NTuple {}) -> existingTuplePort == p
                                                    _                -> False


getOrCreateTuple :: Node.ID -> Graph -> Int -> Either String (Node.ID, Graph)
getOrCreateTuple nodeID graph p = case findMatchingTuple nodeID p graph of 
    Nothing                           -> do let [newTupleID] = Graph.newNodes 1 graph
                                                newTupleNode = NTuple Flags.empty generatedAttrs
                                                newGraphT    = Graph.insEdge (newTupleID, nodeID, Edge p)
                                                             $ Graph.insNode (newTupleID, newTupleNode) graph
                                            return (newTupleID, newGraphT)
    Just (existingTupleID, _, Edge _) -> Right (existingTupleID, graph)


createSelect :: Node.ID -> Graph -> Int -> (Node.ID, Graph)
createSelect nodeID graph p = (selectID, newGraph) where
    selectNode = Expr ("select" ++ show p) Flags.empty $ selectAttrs p
    [selectID] = Graph.newNodes 1 graph
    newGraph = Graph.insEdge (nodeID, selectID, Edge 0)
             $ Graph.insNode (selectID, selectNode) graph


connectGTuples :: (Node.ID, Node.ID, PortDescriptor) -> Int -> Graph -> Either String Graph
connectGTuples (srcNodeID, dstNodeID, dstPorts) p graph =  case dstPorts of 
        []         -> Right $ Graph.insEdge (srcNodeID, dstNodeID, Edge p) graph
        _ -> do
            let dstPortsInit = init dstPorts
            let dstPortsLast = last dstPorts
            (tupleID, graphT) <- getOrCreateTuple dstNodeID graph p
            connectGTuples (srcNodeID, tupleID, dstPortsInit) dstPortsLast graphT

connectG :: (Node.ID, Node.ID, EdgeView) -> Graph -> Either String Graph
connectG (srcNodeID, dstNodeID, EdgeView srcPorts dstPorts) graph = case srcPorts of 
    [] -> connectGTuples (srcNodeID, dstNodeID, dstPorts) 0 graph
    _  -> connectG (selectID, dstNodeID, EdgeView srcPortsTail dstPorts) newGraph where
        srcPortsHead = head srcPorts
        srcPortsTail = tail srcPorts
        (selectID, newGraph) = createSelect srcNodeID graph srcPortsHead



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

    edge2edgeView :: (Node.ID, Node.ID, Edge) -> (Node.ID, Node.ID, EdgeView)
    edge2edgeView (s, d, Edge adst) = case lab graph d of
        Just (NTuple {}) -> (s, d, EdgeView [] [adst])
        _                -> (s, d, EdgeView [] [])
    
    anodes  = labNodes graph
    aedges  = map edge2edgeView $ labEdges graph
    graphv = mkGraph anodes aedges


fromGraph :: Graph -> Either String GraphView
fromGraph graph = do
    let graphC = graph2graphView graph
    foldrM (delGenerated) graphC $ labNodes graphC
    

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
        NTuple {}  -> case (inn graph nodeID, out graph nodeID, suc graph nodeID) of
                        (inEdges, [(_, _, EdgeView _ op)], [adst]) -> Right $ delNode nodeID
                                                                          $ insEdges newEdges graph where 
                                                   newEdges = map (\(asrc, _, EdgeView a ip) -> (asrc, adst, EdgeView a (ip++op))) inEdges
                        _ -> Left "Batch attributes mismatch - incorrectly connected NTuple"
        Default {} -> Right $ delNode nodeID graph
        _          -> case (selectNo node, inn graph nodeID, out graph nodeID) of 
                        (Just num, [(asrc, _, EdgeView isrcPort _)], [(_, adst, EdgeView osrcPort odstPort)])
                            -> Right $ delNode nodeID 
                                     $ insEdge (asrc, adst, EdgeView (isrcPort ++ [num] ++ osrcPort) odstPort) graph
                        _   -> Left "Batch attributes mismatch - incorrectly connected Select"




