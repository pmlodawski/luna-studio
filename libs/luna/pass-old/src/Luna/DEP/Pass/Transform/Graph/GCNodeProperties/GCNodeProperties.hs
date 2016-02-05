---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.DEP.Pass.Transform.Graph.GCNodeProperties.GCNodeProperties where

import           Control.Monad.State
import           Data.IntSet                             ((\\))
import qualified Data.IntSet                             as IntSet

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.Module                     (Module)
import           Luna.DEP.Graph.PropertyMap              (PropertyMap)
import qualified Luna.DEP.Graph.PropertyMap              as PropertyMap
import qualified Luna.DEP.Graph.View.Default.DefaultsMap as DefaultsMap
import qualified Luna.DEP.Graph.View.Default.Expr        as DefaultExpr
import qualified Luna.DEP.Pass.Analysis.ID.ExtractIDs    as ExtractIDs
import           Luna.DEP.Pass.Pass                      (Pass)
import qualified Luna.DEP.Pass.Pass                      as Pass



logger :: LoggerIO
logger = getLoggerIO $moduleName


type GCNodePropertiesPass result = Pass Pass.NoState result


run :: Module -> PropertyMap -> Pass.Result PropertyMap
run = Pass.run_ (Pass.Info "GCNodeProperties") Pass.NoState .: gcIds


gcIds :: Module -> PropertyMap -> GCNodePropertiesPass PropertyMap
gcIds module_ propertyMap = do
    ids <- hoistEither =<< ExtractIDs.runModule module_
    let existingIds      = IntSet.union ids $ IntSet.map (* (-1)) ids
        defaultsMaps     = map (`PropertyMap.getDefaultsMap` propertyMap) $ IntSet.toList existingIds
        defaults         = concatMap DefaultsMap.elems defaultsMaps
        defaultsIds      = IntSet.fromList $ map (view DefaultExpr.nodeID) defaults
    defaultsContentIds <- hoistEither =<< ExtractIDs.runNodeExprs (map (view DefaultExpr.nodeExpr) defaults)
    let pmIds       = PropertyMap.keysSet propertyMap
        orphans     = pmIds \\ IntSet.unions [existingIds, defaultsContentIds, defaultsIds]
    unless (IntSet.null orphans) $
        logger warning $ concat ["GCNodePropertiesPass: found ", show $ IntSet.size orphans, " orphaned ids: ", show orphans]
    return $ foldr PropertyMap.delete propertyMap $ IntSet.toList orphans
