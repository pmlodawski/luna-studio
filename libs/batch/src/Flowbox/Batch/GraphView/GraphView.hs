---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.GraphView(
    addNodesDefaults,
    removeNodesDefaults,
) where

import qualified Data.Map  as Map

import qualified Flowbox.Batch.Batch                as Batch
import qualified Flowbox.Batch.GraphView.Defaults   as Defaults
import           Flowbox.Batch.GraphView.Value      (Value)
import           Flowbox.Control.Error              ()
import           Flowbox.Data.Graph                 hiding (Edge, Graph, empty, fromGraph, sp)
import qualified Flowbox.Luna.Data.Attributes       as Attributes
import           Flowbox.Luna.Data.Graph.Edge       (Edge (Edge))
import qualified Flowbox.Luna.Data.Graph.Flags      as Flags
import           Flowbox.Luna.Data.Graph.Graph      (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph      as Graph
import           Flowbox.Luna.Data.Graph.Node       (Node)
import qualified Flowbox.Luna.Data.Graph.Node       as Node
import           Flowbox.Luna.Data.Graph.Port       (InPort)
import           Flowbox.Luna.Data.Graph.Properties (Properties (Properties))
import qualified Flowbox.Luna.Data.Graph.Properties as Properties
import           Flowbox.Prelude                    hiding (empty)



portMatches :: InPort -> LEdge Edge -> Bool
portMatches newDstPort (_, _, Edge _ connectedDstPort) = newDstPort == connectedDstPort


isNotAlreadyConnected :: Graph -> Node.ID -> InPort -> Bool
isNotAlreadyConnected graph nodeID adstPort = not connected where
    connected = any (portMatches adstPort) (inn graph nodeID)


-------- Conversion to/from Graph --------------------------------------------------------

isGeneratedKey :: String
isGeneratedKey = "DefaultNode-generated"


trueVal :: String
trueVal = "True"


generatedProperties :: Properties
generatedProperties = Properties Flags.empty
                    $ Attributes.fromList [(Batch.attributeKey, Map.fromList [(isGeneratedKey, trueVal)])]


addNodeDefault :: Node.ID -> (InPort, Value) -> Graph -> Graph
addNodeDefault nodeID (adstPort, defaultValue) graph =
    if isNotAlreadyConnected graph nodeID adstPort
        then Graph.connect defaultNodeID nodeID (Edge Nothing adstPort) newGraph
        else graph
    where (newGraph, defaultNodeID) = Graph.insNewNode (Node.Expr defaultValue Nothing generatedProperties) graph


addNodeDefaults :: (Node.ID, Node) -> Graph -> Graph
addNodeDefaults (nodeID, node) graph =
    foldr (addNodeDefault nodeID) graph $ Map.toList defaultsMap where
    defaultsMap = Defaults.getDefaults node



addNodesDefaults :: Graph -> Graph
addNodesDefaults graph =
    foldr addNodeDefaults graph (labNodes graph)


isGenerated :: Node -> Bool
isGenerated node = case getKey $ node ^. (Node.properties . Properties.attrs) of
    Just "True" -> True
    _           -> False
    where getKey = Attributes.get Batch.attributeKey isGeneratedKey


delGenerated :: (Node.ID, Node) -> Graph -> Graph
delGenerated (nodeID, node) graph = if isGenerated node
    then delNode nodeID graph
    else graph


removeNodesDefaults :: Graph -> Graph
removeNodesDefaults graph = foldr delGenerated graph $ labNodes graph


