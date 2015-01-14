---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.NodeDefault where

import           Flowbox.Batch.Batch                     (Batch)
import qualified Flowbox.Batch.Handler.Common            as Batch
import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.AST.Control.Crumb                  (Breadcrumbs)
import qualified Luna.Graph.Flags                        as Flags
import qualified Luna.Graph.Node                         as Node
import           Luna.Graph.Node.Expr                    (NodeExpr)
import qualified Luna.Graph.PropertyMap                  as PropertyMap
import           Luna.Graph.View.Default.DefaultsMap     (DefaultsMap)
import qualified Luna.Graph.View.Default.DefaultsMap     as DefaultsMap
import           Luna.Graph.View.PortDescriptor          (PortDescriptor)
import qualified Luna.Lib.Lib                            as Library
import qualified Luna.Pass.Transform.AST.IDFixer.IDFixer as IDFixer



nodeDefaults :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch DefaultsMap
nodeDefaults nodeID _ libID projectID =
    PropertyMap.getDefaultsMap nodeID <$> Batch.getPropertyMap libID projectID


setNodeDefault :: PortDescriptor -> NodeExpr
               -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setNodeDefault dstPort value nodeID bc libID projectID = do
    propertyMap <- Batch.getPropertyMap libID projectID
    maxID       <- Batch.getMaxID libID projectID
    (fixedValue, newMaxID) <- EitherT $ IDFixer.runNodeExpr' maxID Nothing True value
    let newPM1 = PropertyMap.modifyDefaultsMap (DefaultsMap.insert dstPort (maxID, fixedValue)) nodeID propertyMap
        newPM  = foldr (PropertyMap.modifyFlags (Flags.defaultNodeGenerated .~ Just True)) newPM1 [maxID..newMaxID-1]
    Batch.setPropertyMap newPM libID projectID
    --TODO[PM] : Temporary fix
    Batch.graphViewOp bc libID projectID $ \gv pm -> return ((gv, pm), ())



removeNodeDefault :: PortDescriptor
                  -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
removeNodeDefault dstPort nodeID bc libID projectID = do
    propertyMap <- Batch.getPropertyMap libID projectID
    let newPM = PropertyMap.modifyDefaultsMap (DefaultsMap.delete dstPort) nodeID propertyMap
    Batch.setPropertyMap newPM libID projectID
    --TODO[PM] : Temporary fix
    Batch.graphViewOp bc libID projectID $ \gv pm -> return ((gv, pm), ())

