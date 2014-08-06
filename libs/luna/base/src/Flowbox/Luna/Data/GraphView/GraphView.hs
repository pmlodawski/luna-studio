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
    sort,
) where

import           Data.Foldable (foldlM)
import qualified Data.List     as List
import qualified Data.Maybe    as Maybe
import qualified GHC.Exts      as Exts

import           Flowbox.Data.Graph                         hiding (Edge, Graph, fromGraph)
import qualified Flowbox.Data.Graph                         as DG
import qualified Flowbox.Data.List                          as List
import           Flowbox.Luna.Data.Graph.Edge               (Edge)
import qualified Flowbox.Luna.Data.Graph.Edge               as Edge
import           Flowbox.Luna.Data.Graph.Graph              (Graph)
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
    (graph, newPm) <- foldlM applyEdgeView (DG.mkGraph n [], pm) (DG.labEdges gv)
    return (DG.insEdges (createMonadicEdges graph) graph, newPm)



applyEdgeView :: (Graph, PropertyMap) -> LEdge EdgeView -> Either String (Graph, PropertyMap)
applyEdgeView (graph, pm) (src, dst, edgeview) = case edgeview of
    EdgeView _   []  -> Left "Destination port descriptor should have at leas one element"
    EdgeView []  [d] -> do let newGraph = DG.insEdge (src, dst, Edge.Data  Port.All    d) graph
                           Right (newGraph, pm)
    EdgeView [s] [d] -> do let newGraph = DG.insEdge (src, dst, Edge.Data (Port.Num s) d) graph
                           Right (newGraph, pm)


createMonadicEdges :: Graph -> [LEdge Edge]
createMonadicEdges = List.merge mkMonEdge . map fst . sort where
    mkMonEdge a b = (a, b, Edge.Monadic)


sort :: Graph -> [(Node.ID, Node)]
sort graph = DG.topsortStable graph $ Exts.sortWith Node.position' $ DG.labNodes graph
