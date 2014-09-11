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
import           Luna.Interpreter.RPC.Handler.Lift
import           Luna.Interpreter.Session.Session                               (SessionST)
import qualified Luna.Interpreter.Session.Session                               as Session



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Sync"

--- helpers ---------------------------------------------------------------

syncLibManager :: Int32 -> RPC Context SessionST ()
syncLibManager updateNo = do
    testUpdateNo updateNo
    pm <- Batch.getProjectManager
    activeProjectID <- liftSession Session.getProjectID
    libs <- case ProjectManager.lab pm activeProjectID of
        Just project -> return $ project ^. Project.libs
        Nothing      -> do logger warning $ "Project " ++ show activeProjectID ++ " not found"
                           return def
    liftSession $ Session.setLibManager libs


testUpdateNo :: Int32 -> RPC Context SessionST ()
testUpdateNo updateNo = do
    localUpdateNo <- Batch.getUpdateNo
    assertE (updateNo == localUpdateNo) $
        "UpdateNo does not match (local: " ++ show localUpdateNo ++ ", remote: " ++ show updateNo ++ ")"


testProjectID :: Project.ID -> RPC Context SessionST ()
testProjectID projectID = do
    currentProjectID <- liftSession Session.getProjectID
    assertE (projectID == currentProjectID) $
        "Sync.testProjectID : wrong projectID = " ++ show projectID


hoistSessionST :: RPC Context IO a -> RPC Context SessionST a
hoistSessionST = hoist (hoist liftIO)


sync :: Int32 -> RPC Context IO a -> RPC Context SessionST ()
sync updateNo syncOp = do
    hoistSessionST $ void syncOp
    syncLibManager updateNo

--- handlers --------------------------------------------------------------

projectmanagerSyncGet :: ProjectManagerSyncGet.Status -> RPC Context SessionST ()
projectmanagerSyncGet (ProjectManagerSyncGet.Status _ tdata updateNo) = do
    (projectManager :: ProjectManager) <- hoistEither $ Read.readEither $ decodeP tdata
    Batch.setProjectManager projectManager
    Batch.setUpdateNo updateNo


syncIfNeeded :: RPC Context SessionST () -> RPC Context SessionST (Maybe ProjectManagerSyncGet.Request)
syncIfNeeded rpc = do
    result <- lift $ runEitherT rpc
    case result of
        Right () -> return Nothing
        Left err -> do logger error $ "Not syncing : " ++ err
                       return $ Just ProjectManagerSyncGet.Request


syncRequest :: RPC Context SessionST ProjectManagerSyncGet.Request
syncRequest = return ProjectManagerSyncGet.Request
