---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Properties where

import           Flowbox.Batch.Batch                (Batch)
import           Flowbox.Batch.Handler.Common       (astOp)
import qualified Flowbox.Batch.Handler.Common       as Batch
import qualified Flowbox.Batch.Project.Project      as Project
import qualified Flowbox.Luna.Data.AST.Common       as AST
import           Flowbox.Luna.Data.Graph.Properties (Properties)
import qualified Flowbox.Luna.Data.Graph.Properties as Properties
import qualified Flowbox.Luna.Data.PropertyMap      as PropertyMap
import qualified Flowbox.Luna.Lib.Library           as Library
import           Flowbox.Prelude



getProperties :: AST.ID -> Library.ID -> Project.ID -> Batch Properties
getProperties nodeID libID projectID = do
    propertyMap <- Batch.getPropertyMap libID projectID
    return $ PropertyMap.findWithDefault Properties.empty nodeID propertyMap


setProperties :: Properties -> AST.ID -> Library.ID -> Project.ID -> Batch ()
setProperties properties nodeID libID projectID = astOp libID projectID (\ast propertyMap -> do
    let newPropertyMap = PropertyMap.insert nodeID properties propertyMap
    return ((ast, newPropertyMap), ()))
