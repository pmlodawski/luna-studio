---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Default (
    nodeDefaults,
    setNodeDefault,
    removeNodeDefault,
) where

import qualified Data.Map as Map

import           Flowbox.Batch.Batch                                      (Batch)
import           Flowbox.Batch.Handler.Common                             (graphOp', nodeOp', noresult, readonly)
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Crumb                        (Breadcrumbs)
import qualified Flowbox.Luna.Data.Graph.Graph                            as Graph
import qualified Flowbox.Luna.Data.Graph.Node                             as Node
import           Flowbox.Luna.Data.Graph.Port                             (InPort)
import qualified Flowbox.Luna.Lib.Library                                 as Library
import           Flowbox.Luna.Passes.Transform.Graph.Defaults.DefaultsMap (DefaultsMap)
import qualified Flowbox.Luna.Passes.Transform.Graph.Defaults.DefaultsMap as DefaultsMap
import           Flowbox.Luna.Passes.Transform.Graph.Defaults.Value       (Value)
import           Flowbox.Prelude



nodeDefaults :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO DefaultsMap
nodeDefaults nodeID bc libID projectID  = readonly . nodeOp' nodeID bc libID projectID (\_ node ->
    return (node, DefaultsMap.getDefaults node))


setNodeDefault :: InPort -> Value
               -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
setNodeDefault dstPort value nodeID bc libID projectID = noresult . graphOp' bc libID projectID (\_ graph -> do
    node <- Graph.lab graph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    let newDefaults  = Map.insert dstPort value
                     $ DefaultsMap.getDefaults node
        newNode      = DefaultsMap.setDefaults node newDefaults
        newGraph = Graph.updateNode (nodeID, newNode) graph
    return (newGraph, ()))


removeNodeDefault :: InPort
                  -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNodeDefault dstPort nodeID bc libID projectID = noresult . graphOp' bc libID projectID (\_ graph -> do
    node <- Graph.lab graph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    let newDefaults  = Map.delete dstPort
                     $ DefaultsMap.getDefaults node
        newNode      = DefaultsMap.setDefaults node newDefaults
        newGraph = Graph.updateNode (nodeID, newNode) graph
    return (newGraph, ()))
