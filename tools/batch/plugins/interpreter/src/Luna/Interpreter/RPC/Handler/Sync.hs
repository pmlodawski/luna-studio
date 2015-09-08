---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Luna.Interpreter.RPC.Handler.Sync where

import           Data.Int  (Int32)
import qualified Text.Read as Read

import           "flowbox-utils" Control.Monad.Morph
import qualified Flowbox.Batch.Handler.Common                                   as Batch
import qualified Flowbox.Batch.Project.Project                                  as Project
import           Flowbox.Batch.Project.ProjectManager                           (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                           as ProjectManager
import           Flowbox.Bus.RPC.RPC                                            (RPC)
import           Flowbox.Control.Error                                          hiding (err)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                hiding (Context, error, op)
import           Flowbox.ProjectManager.Context                                 (Context)
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Request as ProjectManagerSyncGet
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Status  as ProjectManagerSyncGet
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.RPC.Handler.Var                               as Var
import qualified Luna.Interpreter.Session.Cache.Invalidate                      as Invalidate
import qualified Luna.Interpreter.Session.Env                                   as Env
import           Luna.Interpreter.Session.Session                               (SessionST)



logger :: LoggerIO
logger = getLoggerIO $moduleName

--- helpers ---------------------------------------------------------------

syncLibManager :: RPC Context (SessionST mm) ()
syncLibManager = do
    pm <- Batch.getProjectManager
    activeProjectID <- liftSession Env.getProjectID
    libs <- case ProjectManager.lab pm activeProjectID of
        Just project -> return $ project ^. Project.libs
        Nothing      -> do logger warning $ "Project " ++ show activeProjectID ++ " not found"
                           return def
    liftSession $ Env.setLibManager libs


testUpdateNo :: Int32 -> RPC Context (SessionST mm) ()
testUpdateNo updateNo = do
    localUpdateNo <- Batch.getUpdateNo
    assertE (updateNo == localUpdateNo) $
        "UpdateNo does not match (local: " ++ show localUpdateNo ++ ", remote: " ++ show updateNo ++ ")"


testProjectID :: Project.ID -> RPC Context (SessionST mm) ()
testProjectID projectID = do
    currentProjectID <- liftSession Env.getProjectID
    assertE (projectID == currentProjectID) $
        "Sync.testProjectID : wrong projectID = " ++ show projectID


hoistSessionST :: RPC Context IO a -> RPC Context (SessionST mm) a
hoistSessionST = hoist (hoist liftIO)


sync :: Int32 -> RPC Context IO a -> RPC Context (SessionST mm) ()
sync updateNo syncOp = do
    hoistSessionST $ void syncOp
    testUpdateNo updateNo
    syncLibManager


--- handlers --------------------------------------------------------------

projectmanagerSyncGet :: ProjectManagerSyncGet.Status -> RPC Context (SessionST mm) ()
projectmanagerSyncGet (ProjectManagerSyncGet.Status _ tdata updateNo) = do
    (projectManager :: ProjectManager) <- hoistEither $ Read.readEither $ decodeP tdata
    Batch.setProjectManager projectManager
    Batch.setUpdateNo updateNo
    syncLibManager
    Var.rebuildTimeRefs
    liftSession Invalidate.modifyAll


syncIfNeeded :: RPC Context (SessionST mm) () -> RPC Context (SessionST mm) (Maybe ProjectManagerSyncGet.Request)
syncIfNeeded rpc = do
    result <- lift $ runEitherT rpc
    case result of
        Right () -> return Nothing
        Left err -> do logger error $ "Not syncing : " ++ err
                       return $ Just ProjectManagerSyncGet.Request


syncRequest :: RPC Context (SessionST mm) ProjectManagerSyncGet.Request
syncRequest = return ProjectManagerSyncGet.Request
