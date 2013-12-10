---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.AST where

import           Flowbox.Batch.Batch                      (Batch)
import qualified Flowbox.Batch.Batch                      as Batch
import           Flowbox.Batch.Handler.Common             (astOp, noresult, readonly)
import qualified Flowbox.Batch.Project.Project            as Project
import           Flowbox.Luna.Data.AST.Crumb.Crumb        (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Module             (Module)
import qualified Flowbox.Luna.Lib.Library                 as Library
import qualified Flowbox.Luna.Passes.Transform.AST.Shrink as Shrink
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.AST"


definitions :: Int -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> Either String Module
definitions maxDepth breadcrumbs libID projectID = readonly . astOp libID projectID (\_ ast -> do
    shrinked <- Shrink.shrinkFunctionBodies ast
    return (ast, shrinked))

