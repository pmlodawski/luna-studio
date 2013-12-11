---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.Graph.Defaults.Defaults (
    addDefaults,
    removeDefaults,
) where

import qualified Data.Map as Map

import           Flowbox.Control.Error                                    ()
import qualified Flowbox.Luna.Data.Attributes                             as Attributes
import           Flowbox.Luna.Data.Graph.Edge                             (Edge (Edge))
import qualified Flowbox.Luna.Data.Graph.Flags                            as Flags
import           Flowbox.Luna.Data.Graph.Graph                            (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                            as Graph
import           Flowbox.Luna.Data.Graph.Node                             (Node)
import qualified Flowbox.Luna.Data.Graph.Node                             as Node
import           Flowbox.Luna.Data.Graph.Port                             (InPort)
import           Flowbox.Luna.Data.Graph.Properties                       (Properties (Properties))
import qualified Flowbox.Luna.Data.Graph.Properties                       as Properties
import qualified Flowbox.Luna.Passes.Transform.Graph.Defaults.DefaultsMap as DefaultsMap
import           Flowbox.Luna.Passes.Transform.Graph.Defaults.Value       (Value)
import           Flowbox.Prelude                                          hiding (empty)




isGeneratedKey :: String
isGeneratedKey = "DefaultNode-generated"


trueVal :: String
trueVal = "True"


generatedProperties :: Properties
generatedProperties = Properties Flags.empty
                    $ Attributes.fromList [(Attributes.lunaAttributeKey, Map.fromList [(isGeneratedKey, trueVal)])]


addDefaults :: Graph -> Graph
addDefaults graph =
    foldr addNodeDefaults graph (Graph.labNodes graph)


addNodeDefaults :: (Node.ID, Node) -> Graph -> Graph
addNodeDefaults (nodeID, node) graph =
    foldr (addNodeDefault nodeID) graph $ Map.toList defaultsMap where
    defaultsMap = DefaultsMap.getDefaults node


addNodeDefault :: Node.ID -> (InPort, Value) -> Graph -> Graph
addNodeDefault nodeID (adstPort, defaultValue) graph =
    if Graph.isNotAlreadyConnected graph nodeID adstPort
        then Graph.connect defaultNodeID nodeID (Edge Nothing adstPort) newGraph
        else graph
    where (newGraph, defaultNodeID) = Graph.insNewNode (Node.Expr defaultValue Nothing generatedProperties) graph


isGenerated :: Node -> Bool
isGenerated node = case getKey $ node ^. (Node.properties . Properties.attrs) of
    Just "True" -> True
    _           -> False
    where getKey = Attributes.get Attributes.lunaAttributeKey isGeneratedKey


delGenerated :: (Node.ID, Node) -> Graph -> Graph
delGenerated (nodeID, node) graph = if isGenerated node
    then Graph.delNode nodeID graph
    else graph


removeDefaults :: Graph -> Graph
removeDefaults graph = foldr delGenerated graph $ Graph.labNodes graph

