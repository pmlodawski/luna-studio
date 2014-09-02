---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Interpreter.RPC.Handler.Sync where

import           Data.Int  (Int32)
import qualified Text.Read as Read

import qualified Flowbox.Batch.Handler.Common                                   as Batch
import qualified Flowbox.Batch.Project.Project                                  as Project
import           Flowbox.Batch.Project.ProjectManager                           (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                           as ProjectManager
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project         ()
import           Flowbox.Bus.RPC.RPC                                            (RPC)
import           Flowbox.Control.Error                                          hiding (err)
import           Flowbox.Control.Monad.Morph
import           Flowbox.Prelude                                                hiding (Context, error, op)
import           Flowbox.ProjectManager.Context                                 (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Request as ProjectManagerSyncGet
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Status  as ProjectManagerSyncGet
import           Luna.Interpreter.Proto.CallPoint                               ()
import           Luna.Interpreter.Proto.CallPointPath                           ()
import qualified Luna.Interpreter.Session.Session                               as Session
import           Luna.Interpreter.Session.SessionT                              (SessionT (SessionT))



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Sync"

--- helpers ---------------------------------------------------------------

syncLibManager :: Int32 -> RPC Context SessionT ()
syncLibManager updateNo = do
    testUpdateNo updateNo
    pm <- Batch.getProjectManager
    activeProjectID <- lift2 $ SessionT $ Session.getProjectID
    project <- ProjectManager.lab pm activeProjectID <??> "Project " ++ show activeProjectID ++ " not found"
    lift2 $ SessionT $ Session.setLibManager $ project ^. Project.libs


testUpdateNo :: Int32 -> RPC Context SessionT ()
testUpdateNo updateNo = do
    localUpdateNo <- Batch.getUpdateNo
    assertE (updateNo == localUpdateNo) $
        "UpdateNo does not match (local: " ++ show localUpdateNo ++ ", remote: " ++ show updateNo ++ ")"


hoistSessionT :: RPC Context IO a -> RPC Context SessionT ()
hoistSessionT = void . hoist ( hoist $ SessionT . liftIO)


sync :: Int32 -> RPC Context IO a -> RPC Context SessionT ()
sync updateNo syncOp = do
    hoistSessionT $ void syncOp
    syncLibManager updateNo

--- handlers --------------------------------------------------------------

projectmanagerSyncGet :: ProjectManagerSyncGet.Status -> RPC Context SessionT ()
projectmanagerSyncGet (ProjectManagerSyncGet.Status _ tdata updateNo) = do
    (projectManager :: ProjectManager) <- hoistEither $ Read.readEither $ decodeP tdata
    Batch.setProjectManager projectManager
    Batch.setUpdateNo updateNo


syncIfNeeded :: RPC Context SessionT () -> RPC Context SessionT (Maybe ProjectManagerSyncGet.Request)
syncIfNeeded rpc = do
    result <- lift $ runEitherT rpc
    case result of
        Right () -> return Nothing
        Left err -> do logger error $ "Not syncing : " ++ err
                       return $ Just ProjectManagerSyncGet.Request


syncRequest :: RPC Context SessionT ProjectManagerSyncGet.Request
syncRequest = return ProjectManagerSyncGet.Request
