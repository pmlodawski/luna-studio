---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Graph.View.GraphView (
    module Flowbox.Data.Graph,
    GraphView,
    fromGraph,
    toGraph,
    isNotAlreadyConnected,
) where

import           Data.Foldable (foldlM)
import qualified Data.List     as List
import qualified Data.Maybe    as Maybe
import           Text.Read     (readMaybe)

import           Flowbox.Control.Error
import           Flowbox.Data.Graph             hiding (Edge, Graph, fromGraph)
import qualified Flowbox.Data.Graph             as DG
import           Flowbox.Data.String            (toUpper)
import           Flowbox.Prelude
import qualified Luna.Graph.Attributes.Naming   as Naming
import qualified Luna.Graph.Edge                as Edge
import           Luna.Graph.Graph               (Graph)
import qualified Luna.Graph.Graph               as Graph
import           Luna.Graph.Node                (Node)
import qualified Luna.Graph.Node                as Node
import qualified Luna.Graph.Node.OutputName     as OutputName
import qualified Luna.Graph.Port                as Port
import           Luna.Graph.PropertyMap         (PropertyMap)
import qualified Luna.Graph.PropertyMap         as PropertyMap
import           Luna.Graph.View.EdgeView       (EdgeView (EdgeView))
import qualified Luna.Graph.View.EdgeView       as EdgeView
import           Luna.Graph.View.PortDescriptor (PortDescriptor)
import qualified Luna.Info                      as Info


type GraphView = DG.Graph Node EdgeView



portMatches :: PortDescriptor -> LEdge EdgeView -> Bool
portMatches adstPort (_, _, connectedPort) = matches where
    connectedDstPort = connectedPort ^. EdgeView.dst
    matches = List.isPrefixOf connectedDstPort adstPort
           || List.isPrefixOf adstPort connectedDstPort


isNotAlreadyConnected :: GraphView -> Node.ID -> PortDescriptor -> Bool
isNotAlreadyConnected graphView nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graphView nodeID)

---------------------------------------------------------------------------

toGraph :: GraphView -> PropertyMap -> Either String (Graph, PropertyMap)
toGraph gv pm = do
    let n = DG.labNodes gv
    (graph, newPM) <- foldlM applyEdgeView (Graph.mkGraph n [], pm) (DG.labEdges gv)
    return (DG.insEdges (Graph.createMonadicEdges graph) graph, newPM)


applyEdgeView :: (Graph, PropertyMap) -> LEdge EdgeView -> Either String (Graph, PropertyMap)
applyEdgeView (graph, pm) (src, dst, edgeview) = do
    node <- Graph.lab graph src <?> "GraphView.applyEdgeView : Cannot find node with id = " ++ show src
    let patternLikeNode = case node of
            Node.Inputs  {}    -> True
            Node.Expr expr _ _ -> (not (null expr)) && (head expr == '=')
            Node.Outputs {}    -> False
    --dtraceM (patternLikeNode, node, edgeview)
    case (patternLikeNode, edgeview) of
        (_   , EdgeView _     [] ) -> Left "Destination port descriptor should have at least one element"
        (_   , EdgeView []    [d]) -> Right (Graph.insEdge (src, dst, Edge.Data  Port.All    d) graph, pm)
        (True, EdgeView [s]   [d]) -> Right (Graph.insEdge (src, dst, Edge.Data (Port.Num s) d) graph, pm)
        (_   , EdgeView (h:t)  d ) -> applyEdgeView (newGraph, newPM) (newNodeID, dst, EdgeView t d) where
            (graph1, newNodeID) = createNode (Get h) graph
            newGraph  = Graph.insEdge (src, newNodeID, Edge.Data Port.All 0) graph1
            newPM     = setGenerated newNodeID pm
        (_   , EdgeView s   d) -> applyEdgeView (newGraph, newPM) (src, newNodeID, EdgeView s $ init d) where
            (graph1, newNodeID) = createNode Tuple graph
            newGraph  = Graph.insEdge (newNodeID, dst, Edge.Data Port.All $ last d) graph1
            newPM     = setGenerated newNodeID pm


createNode :: NodeType -> Graph -> (Graph, Node.ID)
createNode type_ graph = (newGraph, nodeID) where
    nodeID   = DG.newVtx graph
    expr     = case type_ of
                Tuple -> "Tuple"
                Get n -> "get " ++ show n
    node     = OutputName.fixEmpty (Node.Expr expr "" (0, 0)) nodeID
    newGraph = Graph.insNode (nodeID, node) graph


data NodeType = Tuple
              | Get Int
              deriving (Show, Read)

setGenerated :: Node.ID -> PropertyMap -> PropertyMap
setGenerated nodeID =
    PropertyMap.set nodeID (show Info.apiVersion) Naming.graphViewGenerated Naming.true


nodeType :: (Node.ID, Node) -> PropertyMap -> Maybe NodeType
nodeType (nodeID, Node.Expr expr _ _) pm =
    if PropertyMap.get nodeID (show Info.apiVersion) Naming.graphViewGenerated pm == Just Naming.true
        then readMaybe $ toUpper expr
        else Nothing
nodeType  _                           _  = Nothing

---------------------------------------------------------------------------

fromGraph :: Graph -> PropertyMap -> (GraphView, PropertyMap)
fromGraph graph pm = foldl processNode (graphView, pm) $ DG.labNodes graph where
    graphView = mkGraph nodes' edgeviews
    nodes'    = labNodes graph
    edgeviews = Maybe.mapMaybe (\(s, d, e) -> do ev <- EdgeView.fromEdge e
                                                 return (s, d, ev))
                               $ labEdges graph


processNode :: (GraphView, PropertyMap) -> (Node.ID, Node) -> (GraphView, PropertyMap)
processNode (graphView, pm) (nodeID, node) = case nodeType (nodeID, node) pm of
    Nothing    -> (graphView, pm)
    Just type_ -> (newGraphView, newPM) where
        inEdges  = DG.inn graphView nodeID
        outEdges = DG.out graphView nodeID
        newEdges = mergeEdges type_ <$> inEdges <*> outEdges
        newGraphView = DG.insEdges newEdges
                     $ DG.delNode nodeID graphView
        newPM = PropertyMap.delete nodeID pm


mergeEdges :: NodeType -> (Node.ID, Node.ID, EdgeView) -> (Node.ID, Node.ID, EdgeView) -> (Node.ID, Node.ID, EdgeView)
mergeEdges Tuple     (src, _, EdgeView s1 d1) (_, dst, EdgeView _ d2) =
    (src, dst, EdgeView s1 (d1 ++ d2) )
mergeEdges (Get num) (src, _, EdgeView s1 _) (_, dst, EdgeView _ d2) =
    (src, dst, EdgeView (s1 ++ [num]) d2)
