---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.Sync where

import qualified Flowbox.Batch.Handler.Common                                   as Batch
import           Flowbox.Bus.RPC.RPC                                            (RPC)
import           Flowbox.Prelude                                                hiding (Context)
import           Flowbox.ProjectManager.Context                                 (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Request as SyncGet
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Status  as SyncGet


logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Sync"

------ public api -------------------------------------------------


syncGet :: SyncGet.Request -> RPC Context IO SyncGet.Status
syncGet request = do
    projectManager <- Batch.getProjectManager
    return $ SyncGet.Status request $ encodeP $ show projectManager
