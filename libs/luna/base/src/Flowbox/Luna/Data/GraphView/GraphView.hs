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

import qualified Data.List as List

import           Flowbox.Data.Graph                         hiding (Edge, Graph, fromGraph)
import qualified Flowbox.Data.Graph                         as DG
import           Flowbox.Luna.Data.Graph.Graph              (Graph)
import           Flowbox.Luna.Data.Graph.Node               (Node)
import qualified Flowbox.Luna.Data.Graph.Node               as Node
import           Flowbox.Luna.Data.GraphView.EdgeView       (EdgeView)
import qualified Flowbox.Luna.Data.GraphView.EdgeView       as EdgeView
import           Flowbox.Luna.Data.GraphView.PortDescriptor (PortDescriptor)
import           Flowbox.Prelude



type GraphView = DG.Graph Node EdgeView


fromGraph :: Graph -> GraphView
fromGraph = DG.emap EdgeView.fromEdge


toGraph :: (Applicative m, Monad m) => GraphView -> m Graph
toGraph gv = do let n = labNodes gv
                ev <- mapM EdgeView.toLEdge $ labEdges gv
                return $ mkGraph n ev


portMatches :: PortDescriptor -> LEdge EdgeView -> Bool
portMatches adstPort (_, _, connectedPort) = matches where
    connectedDstPort = connectedPort ^. EdgeView.dst
    matches = List.isPrefixOf connectedDstPort adstPort
           || List.isPrefixOf adstPort connectedDstPort


isNotAlreadyConnected :: GraphView -> Node.ID -> PortDescriptor -> Bool
isNotAlreadyConnected graphview nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graphview nodeID)

