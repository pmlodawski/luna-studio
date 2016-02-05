---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Syntax.Graph.View.GraphView (
    module Flowbox.Data.Graph,
    GraphView,
    fromGraph,
    toGraph,
    isNotAlreadyConnected,
) where

import           Data.Foldable                         (foldlM)
import qualified Data.List                             as List
import qualified Data.Maybe                            as Maybe

import           Flowbox.Control.Error
import           Flowbox.Data.Graph                    hiding (Edge, Graph, fromGraph)
import qualified Flowbox.Data.Graph                    as DG
import           Flowbox.Prelude
import qualified Luna.Syntax.Graph.Edge                as Edge
import           Luna.Syntax.Graph.Graph               (Graph)
import qualified Luna.Syntax.Graph.Graph               as Graph
import           Luna.Syntax.Graph.Node                (Node)
import qualified Luna.Syntax.Graph.Node                as Node
import           Luna.Syntax.Graph.Node.Expr           (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr           as NodeExpr
import qualified Luna.Syntax.Graph.Node.OutputPat      as OutputPat
import qualified Luna.Syntax.Graph.Node.StringExpr     as StringExpr
import qualified Luna.Syntax.Graph.Port                as Port
import           Luna.Syntax.Graph.Tag                 (Tag)
import           Luna.Syntax.Graph.View.EdgeView       (EdgeView (EdgeView))
import qualified Luna.Syntax.Graph.View.EdgeView       as EdgeView
import           Luna.Syntax.Graph.View.PortDescriptor (PortDescriptor)



type GraphView a v = DG.Graph (Node a v) EdgeView


portMatches :: PortDescriptor -> LEdge EdgeView -> Bool
portMatches adstPort (_, _, connectedPort) = matches where
    connectedDstPort = connectedPort ^. EdgeView.dst
    matches = List.isPrefixOf connectedDstPort adstPort
           || List.isPrefixOf adstPort connectedDstPort


isNotAlreadyConnected :: GraphView a v -> Node.ID -> PortDescriptor -> Bool
isNotAlreadyConnected graphView nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graphView nodeID)

-----------------------------------------------------------------------------

toGraph :: (Eq a, Eq v) => GraphView a v -> Either String (Graph a v)
toGraph gv = do
    let n = DG.labNodes gv
    graph <- foldlM applyEdgeView (Graph.mkGraph n []) (DG.labEdges gv)
    return $ DG.insEdges (Graph.createMonadicEdges graph) graph


applyEdgeView :: Graph a v -> LEdge EdgeView -> Either String (Graph a v)
applyEdgeView graph (src, dst, edgeview) = do
    srcNode <- Graph.lab graph src <?> "GraphView.applyEdgeView : Cannot find node with id = " ++ show src
    dstNode <- Graph.lab graph src <?> "GraphView.applyEdgeView : Cannot find node with id = " ++ show src
    let patternLikeNode = case srcNode of
            Node.Inputs  {}                                                      -> True
            Node.Expr    (NodeExpr.StringExpr (StringExpr.Pattern {})) _ _ _ _ _ -> True
            Node.Expr    {}                                                      -> False
            Node.Outputs {}                                                      -> False
    case (patternLikeNode || Node.isOutputs dstNode, edgeview) of
        (_   , EdgeView []    [] ) -> Right $ Graph.insEdge (src, dst, Edge.Data  Port.mkSrcAll  Port.mkDstAll) graph
        (_   , EdgeView []    [d]) -> Right $ Graph.insEdge (src, dst, Edge.Data  Port.mkSrcAll (Port.mkDst d)) graph
        (_   , EdgeView [s]   [] ) -> Right $ Graph.insEdge (src, dst, Edge.Data (Port.mkSrc s)  Port.mkDstAll) graph
        (True, EdgeView [s]   [d]) -> Right $ Graph.insEdge (src, dst, Edge.Data (Port.mkSrc s) (Port.mkDst d)) graph
        (_   , EdgeView (h:t)  d ) -> applyEdgeView newGraph (newNodeID, dst, EdgeView t d) where
            (graph1, newNodeID) = createNode (NodeExpr.StringExpr $ StringExpr.Get $ show h) graph
            newGraph  = Graph.insEdge (src, newNodeID, Edge.Data Port.mkSrcAll $ Port.mkDst 0) graph1
        (_   , EdgeView []   d) -> applyEdgeView newGraph (src, newNodeID, EdgeView [] $ init d) where
            (graph1, newNodeID) = createNode (NodeExpr.StringExpr StringExpr.Tuple) graph
            newGraph  = Graph.insEdge (newNodeID, dst, Edge.Data Port.mkSrcAll $ Port.mkDst $ last d) graph1


createNode :: NodeExpr a v -> Graph a v -> (Graph a v, Node.ID)
createNode nodeExpr graph = (newGraph, nodeID) where
    nodeID   = DG.newVtx graph
    node     = Node.Expr nodeExpr Nothing def (0, 0) [] True
    newGraph = Graph.insNode (nodeID, node) graph


nodeType :: Node a v -> Maybe (NodeExpr a v)
nodeType (Node.Expr expr _ _ _ _ True) = Just expr
nodeType  _                            = Nothing

-----------------------------------------------------------------------------

fromGraph :: Graph a v -> GraphView a v
fromGraph graph = foldl processNode graphView $ DG.labNodes graph where
    graphView = mkGraph nodes' edgeviews
    nodes'    = labNodes graph
    edgeviews = Maybe.mapMaybe (\(s, d, e) -> do ev <- EdgeView.fromEdge e
                                                 return (s, d, ev))
                               $ labEdges graph


processNode :: GraphView a v -> (Node.ID, Node a v) -> GraphView a v
processNode graphView (nodeID, node) = case nodeType node of
    Nothing    -> graphView
    Just type_ -> newGraphView where
        inEdges  = DG.inn graphView nodeID
        outEdges = DG.out graphView nodeID
        newEdges = mergeEdges type_ <$> inEdges <*> outEdges
        newGraphView = DG.insEdges newEdges
                     $ DG.delNode nodeID graphView

mergeEdges :: NodeExpr a v -> (Node.ID, Node.ID, EdgeView) -> (Node.ID, Node.ID, EdgeView) -> (Node.ID, Node.ID, EdgeView)
mergeEdges (NodeExpr.StringExpr StringExpr.Tuple)     (src, _, EdgeView s1 d1) (_, dst, EdgeView _ d2) =
    (src, dst, EdgeView s1 (d1 ++ d2) )
mergeEdges (NodeExpr.StringExpr (StringExpr.Get num)) (src, _, EdgeView s1 _) (_, dst, EdgeView _ d2) =
    (src, dst, EdgeView (s1 ++ [read num]) d2)
