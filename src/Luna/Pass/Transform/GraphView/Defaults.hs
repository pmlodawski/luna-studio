---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.Transform.GraphView.Defaults (
    addDefaults,
    removeDefaults,
    isGenerated,
) where

import qualified Data.Map as Map

import           Flowbox.Control.Error                               ()
import qualified Flowbox.Luna.Data.Attributes                        as Attributes
import qualified Flowbox.Luna.Data.Graph.Flags                       as Flags
import qualified Flowbox.Luna.Data.Graph.Node                        as Node
import           Flowbox.Luna.Data.Graph.Properties                  (Properties (Properties))
import qualified Flowbox.Luna.Data.GraphView.Default.DefaultsMap     as DefaultsMap
import           Flowbox.Luna.Data.GraphView.Default.Value           (Value)
import           Flowbox.Luna.Data.GraphView.EdgeView                (EdgeView (EdgeView))
import           Flowbox.Luna.Data.GraphView.GraphView               (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView               as GraphView
import           Flowbox.Luna.Data.GraphView.PortDescriptor          (PortDescriptor)
import           Flowbox.Luna.Data.PropertyMap                       (PropertyMap)
import qualified Flowbox.Luna.Data.PropertyMap                       as PropertyMap
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes      as Attributes
import qualified Flowbox.Luna.Passes.Transform.Graph.Node.OutputName as OutputName
import           Flowbox.Prelude                                     hiding (empty)


generatedProperties :: Properties
generatedProperties = Properties Flags.empty
                    $ Attributes.fromList [( Attributes.luna
                                           , Map.fromList [( Attributes.defaultNodeGenerated
                                                           , Attributes.true
                                                           )]
                                           )]


addDefaults :: GraphView -> PropertyMap -> (GraphView, PropertyMap)
addDefaults graph propertyMap =
    foldr addNodeDefaults (graph, propertyMap) $ GraphView.nodes graph


addNodeDefaults :: Node.ID -> (GraphView, PropertyMap) -> (GraphView, PropertyMap)
addNodeDefaults nodeID gp@(_, propertyMap) =
    foldr (addNodeDefault nodeID) gp defaults
    where
        defaults = Map.toList $ DefaultsMap.getDefaultsMap nodeID propertyMap


addNodeDefault :: Node.ID -> (PortDescriptor, (Node.ID, Value)) -> (GraphView, PropertyMap) -> (GraphView, PropertyMap)
addNodeDefault nodeID (adstPort, (defaultNodeID, defaultValue)) (graph, propertyMap) =
    if GraphView.isNotAlreadyConnected graph nodeID adstPort
        then (newGraph2, newPropertyMap)
        else (graph, propertyMap)
    where
      node      = Node.Expr defaultValue (OutputName.generate defaultValue nodeID) (0, 0)
      newGraph  = GraphView.insNode (defaultNodeID, node) graph
      newGraph2 = GraphView.insEdge (defaultNodeID, nodeID, EdgeView [] adstPort) newGraph
      newPropertyMap = PropertyMap.insert defaultNodeID generatedProperties propertyMap


isGenerated :: Node.ID -> PropertyMap -> Bool
isGenerated nodeID propertyMap =
    PropertyMap.get nodeID Attributes.luna Attributes.defaultNodeGenerated propertyMap == Just "True"


delGenerated :: Node.ID -> (GraphView, PropertyMap) -> (GraphView, PropertyMap)
delGenerated nodeID gp@(graph, propertyMap) = if isGenerated nodeID propertyMap
    then (GraphView.delNode nodeID graph, PropertyMap.delete nodeID propertyMap)
    else gp


removeDefaults :: GraphView -> PropertyMap -> (GraphView, PropertyMap)
removeDefaults graph propertyMap = foldr delGenerated (graph, propertyMap) $ GraphView.nodes graph

