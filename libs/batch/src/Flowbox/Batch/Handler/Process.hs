---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Process  where

import           Flowbox.Batch.Batch           (Batch)
import qualified Flowbox.Batch.Batch           as Batch
import           Flowbox.Batch.Handler.Common  (libManagerOp, libraryOp, noresult, readonly)
import qualified Flowbox.Batch.Process.Process as Process
import qualified Flowbox.Batch.Project.Project as Project
import qualified Flowbox.Luna.Lib.Library      as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform       as Platform
import qualified Flowbox.System.Process        as Process
import           Flowbox.System.UniPath        (UniPath)
import qualified Flowbox.System.UniPath        as UniPath


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Process"


processes :: (Applicative m, Monad m) => Project.ID -> Batch -> m [Process.ID]
processes = undefined
