---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.NodeDefault where

import           Flowbox.Batch.Batch                             (Batch)
import qualified Flowbox.Batch.Handler.Common                    as Batch
import qualified Flowbox.Batch.Project.Project                   as Project
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs         (Breadcrumbs)
import qualified Flowbox.Luna.Data.Graph.Node                    as Node
import           Flowbox.Luna.Data.GraphView.Default.DefaultsMap (DefaultsMap)
import qualified Flowbox.Luna.Data.GraphView.Default.DefaultsMap as DefaultsMap
import           Flowbox.Luna.Data.GraphView.Default.Value       (Value)
import           Flowbox.Luna.Data.GraphView.PortDescriptor      (PortDescriptor)
import qualified Flowbox.Luna.Lib.Library                        as Library
import           Flowbox.Prelude



nodeDefaults :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch DefaultsMap
nodeDefaults nodeID _ libID projectID =
    DefaultsMap.getDefaultsMap nodeID <$> Batch.getPropertyMap libID projectID


setNodeDefault :: PortDescriptor -> Value
               -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setNodeDefault dstPort value nodeID _ libID projectID = Batch.propertyMapOp libID projectID (\propertyMap -> do
    maxID <- Batch.getMaxID libID projectID
    return (DefaultsMap.addDefault dstPort (maxID, value) nodeID propertyMap, ()))


removeNodeDefault :: PortDescriptor
                  -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
removeNodeDefault dstPort nodeID _ libID projectID = Batch.propertyMapOp libID projectID (\propertyMap ->
    return (DefaultsMap.removeDefault dstPort nodeID propertyMap, ()))

