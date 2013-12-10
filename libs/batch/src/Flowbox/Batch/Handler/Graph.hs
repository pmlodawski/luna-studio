---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Graph where

import           Flowbox.Batch.Batch               (Batch)
import qualified Flowbox.Batch.Batch               as Batch
import           Flowbox.Batch.Handler.Common      (graphOp', noresult, readonly')
import qualified Flowbox.Batch.Project.Project     as Project
import           Flowbox.Luna.Data.AST.Crumb.Crumb (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Module      (Module)
import           Flowbox.Luna.Data.Graph.Graph     (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph     as Graph
import qualified Flowbox.Luna.Lib.Library          as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Graph"


graph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Graph
graph bc libID projectID = readonly' . graphOp' bc libID projectID (\_ graph -> do
    return (graph, graph))
