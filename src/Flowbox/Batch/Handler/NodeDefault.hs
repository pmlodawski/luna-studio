---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.NodeDefault where

import           Flowbox.Batch.Batch                 (Batch)
import qualified Flowbox.Batch.Handler.Common        as Batch
import qualified Flowbox.Batch.Project.Project       as Project
import           Flowbox.Prelude
import           Luna.AST.Control.Crumb              (Breadcrumbs)
import qualified Luna.Graph.Node                     as Node
import           Luna.Graph.View.Default.DefaultsMap (DefaultsMap)
import qualified Luna.Graph.View.Default.DefaultsMap as DefaultsMap
import           Luna.Graph.View.Default.Value       (Value)
import           Luna.Graph.View.PortDescriptor      (PortDescriptor)
import qualified Luna.Lib.Lib                        as Library



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

