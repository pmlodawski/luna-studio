---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.NodeDefault where

import           Flowbox.Batch.Batch                             (Batch)
import           Flowbox.Batch.Handler.Common                    (graphViewOp, noresult, readonly)
import qualified Flowbox.Batch.Project.Project                   as Project
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs         (Breadcrumbs)
import qualified Flowbox.Luna.Data.Graph.Node                    as Node
import           Flowbox.Luna.Data.GraphView.Default.DefaultsMap (DefaultsMap)
import qualified Flowbox.Luna.Data.GraphView.Default.DefaultsMap as DefaultsMap
import           Flowbox.Luna.Data.GraphView.Default.Value       (Value)
import           Flowbox.Luna.Data.GraphView.PortDescriptor      (PortDescriptor)
import qualified Flowbox.Luna.Lib.Library                        as Library
import           Flowbox.Prelude



nodeDefaults :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO DefaultsMap
nodeDefaults nodeID bc libID projectID  = readonly . graphViewOp bc libID projectID (\_ graph propertyMap _ -> do
    return ((graph, propertyMap), DefaultsMap.getDefaultsMap nodeID propertyMap))


setNodeDefault :: PortDescriptor -> Value
               -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
setNodeDefault dstPort value nodeID bc libID projectID = noresult . graphViewOp bc libID projectID (\_ graph propertyMap maxID ->
    return ((graph, DefaultsMap.addDefault dstPort (maxID, value) nodeID propertyMap), ()))


removeNodeDefault :: PortDescriptor
                  -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
removeNodeDefault dstPort nodeID bc libID projectID = noresult . graphViewOp bc libID projectID (\_ graph propertyMap _ ->
    return ((graph, DefaultsMap.removeDefault dstPort nodeID propertyMap), ()))
