---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.GraphView.GraphView (
    module Flowbox.Data.Graph,
    GraphView,
    fromGraph,
    toGraph,
    isNotAlreadyConnected,
) where

import           Data.Foldable (foldlM)
import qualified Data.List     as List
import qualified Data.Maybe    as Maybe

import           Flowbox.Data.Graph                         hiding (Edge, Graph, fromGraph)
import qualified Flowbox.Data.Graph                         as DG
import qualified Flowbox.Luna.Data.Graph.Edge               as Edge
import           Flowbox.Luna.Data.Graph.Graph              (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph              as Graph
import           Flowbox.Luna.Data.Graph.Node               (Node)
import qualified Flowbox.Luna.Data.Graph.Node               as Node
import qualified Flowbox.Luna.Data.Graph.Port               as Port
import           Flowbox.Luna.Data.GraphView.EdgeView       (EdgeView (EdgeView))
import qualified Flowbox.Luna.Data.GraphView.EdgeView       as EdgeView
import           Flowbox.Luna.Data.GraphView.PortDescriptor (PortDescriptor)
import           Flowbox.Luna.Data.PropertyMap              (PropertyMap)
import           Flowbox.Prelude



type GraphView = DG.Graph Node EdgeView


fromGraph :: Graph -> GraphView
fromGraph graph = mkGraph nodes' edgeviews where
    nodes'    = labNodes graph
    edgeviews = Maybe.mapMaybe (\(s, d, e) -> do ev <- EdgeView.fromEdge e
                                                 return (s, d, ev))
                               $ labEdges graph


portMatches :: PortDescriptor -> LEdge EdgeView -> Bool
portMatches adstPort (_, _, connectedPort) = matches where
    connectedDstPort = connectedPort ^. EdgeView.dst
    matches = List.isPrefixOf connectedDstPort adstPort
           || List.isPrefixOf adstPort connectedDstPort


isNotAlreadyConnected :: GraphView -> Node.ID -> PortDescriptor -> Bool
isNotAlreadyConnected graphview nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graphview nodeID)

---------------------------------------------------------------------------

toGraph :: GraphView -> PropertyMap -> Either String (Graph, PropertyMap)
toGraph gv pm = do
    let n = DG.labNodes gv
    (graph, newPm) <- foldlM applyEdgeView (Graph.mkGraph n [], pm) (DG.labEdges gv)
    return (DG.insEdges (Graph.createMonadicEdges graph) graph, newPm)


applyEdgeView :: (Graph, PropertyMap) -> LEdge EdgeView -> Either String (Graph, PropertyMap)
applyEdgeView (graph, pm) (src, dst, edgeview) = case edgeview of
    EdgeView _     []  -> Left "Destination port descriptor should have at least one element"
    EdgeView []    [d] -> Right (Graph.insEdge (src, dst, Edge.Data  Port.All    d) graph, pm)
    EdgeView [s]   [d] -> Right (Graph.insEdge (src, dst, Edge.Data (Port.Num s) d) graph, pm)
    EdgeView (h:t)  d  -> applyEdgeView (newGraph, newPm) (getNodeID, dst, EdgeView t d) where
        getNode             = Node.Expr ("get" ++ show h) "" (0, 0)
        (graph1, getNodeID) = Graph.insNewNode getNode graph
        newGraph            = Graph.insEdge (src, getNodeID, Edge.Data (Port.Num h) 0) graph1
        newPm               = pm




