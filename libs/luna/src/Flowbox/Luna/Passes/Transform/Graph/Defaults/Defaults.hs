---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.Graph.Defaults.Defaults (
    addDefaults,
    removeDefaults,
    isGenerated,
) where

import qualified Data.Map as Map

import           Flowbox.Control.Error                          ()
import qualified Flowbox.Luna.Data.Attributes                   as Attributes
import qualified Flowbox.Luna.Data.Graph.Default.DefaultsMap    as DefaultsMap
import           Flowbox.Luna.Data.Graph.Default.Value          (Value)
import           Flowbox.Luna.Data.Graph.Edge                   (Edge (Edge))
import qualified Flowbox.Luna.Data.Graph.Flags                  as Flags
import           Flowbox.Luna.Data.Graph.Graph                  (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                  as Graph
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import           Flowbox.Luna.Data.Graph.Port                   (InPort)
import qualified Flowbox.Luna.Data.Graph.Port                   as Port
import           Flowbox.Luna.Data.Graph.Properties             (Properties (Properties))
import           Flowbox.Luna.Data.PropertyMap                  (PropertyMap)
import qualified Flowbox.Luna.Data.PropertyMap                  as PropertyMap
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes as Attributes
import           Flowbox.Prelude                                hiding (empty)



generatedProperties :: Properties
generatedProperties = Properties Flags.empty
                    $ Attributes.fromList [( Attributes.luna
                                           , Map.fromList [( Attributes.defaultNodeGenerated
                                                           , Attributes.true
                                                           )]
                                           )]


addDefaults :: Graph -> PropertyMap -> (Graph, PropertyMap)
addDefaults graph propertyMap =
    foldr addNodeDefaults (graph, propertyMap) $ Graph.nodes graph


addNodeDefaults :: Node.ID -> (Graph, PropertyMap) -> (Graph, PropertyMap)
addNodeDefaults nodeID gp@(_, propertyMap) =
    foldr (addNodeDefault nodeID) gp defaults
    where
        defaults = Map.toList $ DefaultsMap.getDefaultsMap nodeID propertyMap


addNodeDefault :: Node.ID -> (InPort, (Node.ID, Value)) -> (Graph, PropertyMap) -> (Graph, PropertyMap)
addNodeDefault nodeID (adstPort, (defaultNodeID, defaultValue)) (graph, propertyMap) =
    if Graph.isNotAlreadyConnected graph nodeID adstPort
        then (newGraph2, newPropertyMap)
        else (graph, propertyMap)
    where
      newGraph = Graph.insNode (defaultNodeID, Node.Expr defaultValue defaultValue) graph
      newGraph2 = Graph.connect defaultNodeID nodeID (Edge Port.All adstPort) newGraph
      newPropertyMap = PropertyMap.insert defaultNodeID generatedProperties propertyMap



isGenerated :: Node.ID -> PropertyMap -> Bool
isGenerated nodeID propertyMap = case PropertyMap.get nodeID Attributes.luna Attributes.defaultNodeGenerated propertyMap of
    Just "True" -> True
    _           -> False


delGenerated :: Node.ID -> (Graph, PropertyMap) -> (Graph, PropertyMap)
delGenerated nodeID gp@(graph, propertyMap) = if isGenerated nodeID propertyMap
    then (Graph.delNode nodeID graph, PropertyMap.delete nodeID propertyMap)
    else gp


removeDefaults :: Graph -> PropertyMap -> (Graph, PropertyMap)
removeDefaults graph propertyMap = foldr delGenerated (graph, propertyMap) $ Graph.nodes graph

