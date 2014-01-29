---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Properties (
    getProperties,
    setProperties,
) where

import           Flowbox.Batch.Batch                (Batch)
import           Flowbox.Batch.Handler.Common       (astOp, noresult, readonly)
import qualified Flowbox.Batch.Project.Project      as Project
import qualified Flowbox.Luna.Data.AST.Common       as AST
import           Flowbox.Luna.Data.Graph.Properties (Properties)
import qualified Flowbox.Luna.Data.Graph.Properties as Properties
import qualified Flowbox.Luna.Data.PropertyMap      as PropertyMap
import qualified Flowbox.Luna.Lib.Library           as Library
import           Flowbox.Prelude



getProperties :: AST.ID -> Library.ID -> Project.ID -> Batch -> IO Properties
getProperties nodeID libID projectID  = readonly . astOp libID projectID (\_ ast propertyMap -> do
    let properties = PropertyMap.findWithDefault Properties.empty nodeID propertyMap
    return ((ast, propertyMap), properties))



setProperties :: Properties -> AST.ID -> Library.ID -> Project.ID -> Batch -> IO Batch
setProperties properties nodeID libID projectID  = noresult . astOp libID projectID (\_ ast propertyMap -> do
    let newPropertyMap = PropertyMap.insert nodeID properties propertyMap
    return ((ast, newPropertyMap), ()))

