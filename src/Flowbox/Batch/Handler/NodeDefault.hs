---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.NodeDefault where

import           Flowbox.Batch.Batch                         (Batch)
import qualified Flowbox.Batch.Handler.Common                as Batch
import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.DEP.AST.Control.Crumb                  (Breadcrumbs)
import qualified Luna.DEP.Data.ASTInfo                       as ASTInfo
import qualified Luna.DEP.Graph.Flags                        as Flags
import qualified Luna.DEP.Graph.Node                         as Node
import           Luna.DEP.Graph.Node.Expr                    (NodeExpr)
import qualified Luna.DEP.Graph.PropertyMap                  as PropertyMap
import           Luna.DEP.Graph.View.Default.DefaultsMap     (DefaultsMap)
import qualified Luna.DEP.Graph.View.Default.DefaultsMap     as DefaultsMap
import           Luna.DEP.Graph.View.Default.Expr            (DefaultExpr (DefaultExpr))
import           Luna.DEP.Graph.View.PortDescriptor          (PortDescriptor)
import qualified Luna.DEP.Lib.Lib                            as Library
import qualified Luna.DEP.Pass.Transform.AST.IDFixer.IDFixer as IDFixer



nodeDefaults :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch DefaultsMap
nodeDefaults nodeID _ libraryID projectID =
    PropertyMap.getDefaultsMap nodeID <$> Batch.getPropertyMap libraryID projectID


setNodeDefault :: PortDescriptor -> NodeExpr
               -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setNodeDefault dstPort value nodeID bc libraryID projectID = do
    propertyMap <- Batch.getPropertyMap libraryID projectID
    astInfo     <- Batch.getASTInfo libraryID projectID
    (fixedValue, astInfo') <- EitherT $ IDFixer.runNodeExpr astInfo Nothing True value
    let newID    = astInfo ^. ASTInfo.lastID
        newMaxID = astInfo' ^. ASTInfo.lastID
        updateDefaultsMap  Nothing                          = Just (DefaultExpr newID newID    fixedValue)
        updateDefaultsMap (Just (DefaultExpr _ originID _)) = Just (DefaultExpr newID originID fixedValue)
        newPM1   = PropertyMap.modifyDefaultsMap (DefaultsMap.alter updateDefaultsMap dstPort) nodeID propertyMap
        newPM    = foldr (PropertyMap.modifyFlags (Flags.defaultNodeGenerated .~ Just True)) newPM1 [newID..newMaxID-1]
    Batch.setASTInfo astInfo' libraryID projectID
    Batch.setPropertyMap newPM libraryID projectID
    --TODO[PM] : Temporary fix
    Batch.graphViewOp bc libraryID projectID $ \gv pm -> return ((gv, pm), ())



removeNodeDefault :: PortDescriptor
                  -> Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
removeNodeDefault dstPort nodeID bc libraryID projectID = do
    propertyMap <- Batch.getPropertyMap libraryID projectID
    let newPM = PropertyMap.modifyDefaultsMap (DefaultsMap.delete dstPort) nodeID propertyMap
    Batch.setPropertyMap newPM libraryID projectID
    --TODO[PM] : Temporary fix
    Batch.graphViewOp bc libraryID projectID $ \gv pm -> return ((gv, pm), ())
