---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Defaults (
    nodeDefaults,
    setNodeDefault,
    removeNodeDefault,
) where

import qualified Data.Map                               as Map

import           Flowbox.Prelude                          
import           Flowbox.Batch.Batch                      (Batch(..))
import qualified Flowbox.Batch.GraphView.Defaults       as Defaults
import           Flowbox.Batch.GraphView.Defaults         (DefaultsMap)
import qualified Flowbox.Batch.GraphView.GraphView      as GraphView
import           Flowbox.Batch.GraphView.PortDescriptor   (PortDescriptor)
import           Flowbox.Batch.Handlers.Common            (noresult, readonly, graphOp, nodeOp)
import qualified Flowbox.Batch.Project.Project          as Project
import           Flowbox.Control.Error                    ((<?>))
import qualified Flowbox.Luna.Lib.Library               as Library
import qualified Flowbox.Luna.Network.Def.Definition    as Definition
import           Flowbox.Luna.Network.Graph.Value         (Value)
import qualified Flowbox.Luna.Network.Graph.Node        as Node



nodeDefaults :: Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String DefaultsMap
nodeDefaults nodeID defID libID projectID  = readonly . nodeOp nodeID defID libID projectID (\_ node -> let
    defaults = Defaults.getDefaults node
    in Right (node, defaults))


setNodeDefault :: PortDescriptor -> Value
               -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
setNodeDefault dstPort value nodeID defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do
    graphview <- GraphView.fromGraph agraph
    node <- GraphView.lab graphview nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    let newDefaults  = Map.insert dstPort value
                     $ Defaults.getDefaults node
        newNode      = Defaults.setDefaults node newDefaults
        newGraphView = GraphView.updateNode (nodeID, newNode) graphview
    newGraph <- GraphView.toGraph newGraphView
    return (newGraph, ()))


removeNodeDefault :: PortDescriptor
                  -> Node.ID -> Definition.ID -> Library.ID -> Project.ID -> Batch -> Either String Batch
removeNodeDefault dstPort nodeID defID libID projectID = noresult . graphOp defID libID projectID (\_ agraph -> do
    graphview <- GraphView.fromGraph agraph
    node <- GraphView.lab graphview nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    let newDefaults  = Map.delete dstPort
                     $ Defaults.getDefaults node
        newNode      = Defaults.setDefaults node newDefaults
        newGraphView = GraphView.updateNode (nodeID, newNode) graphview
    newGraph <- GraphView.toGraph newGraphView
    return (newGraph, ()))