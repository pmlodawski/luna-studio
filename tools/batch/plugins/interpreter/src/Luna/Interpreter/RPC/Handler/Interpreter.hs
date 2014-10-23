---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Luna.Interpreter.RPC.Handler.Interpreter where

import qualified Control.Concurrent as Concurrent

import           Flowbox.Bus.RPC.RPC                                                          (RPC)
import           Flowbox.Control.Error
import qualified Flowbox.Data.SetForest                                                       as SetForest
import           Flowbox.Prelude                                                              hiding (Context)
import           Flowbox.ProjectManager.Context                                               (Context)
import           Flowbox.System.Log.Logger                                                    hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Interpreter.GetMainPtr.Request                   as GetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.GetMainPtr.Status                    as GetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.GetProjectID.Request                 as GetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.GetProjectID.Status                  as GetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.Ping.Request                         as Ping
import qualified Generated.Proto.Interpreter.Interpreter.Ping.Status                          as Ping
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request                          as Run
import qualified Generated.Proto.Interpreter.Interpreter.Run.Update                           as Run
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.DefaultGet.Request as GetDefaultSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.DefaultGet.Status  as GetDefaultSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.DefaultSet.Request as SetDefaultSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.DefaultSet.Update  as SetDefaultSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Delete.Request     as DeleteSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Delete.Update      as DeleteSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Get.Request        as GetSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Get.Status         as GetSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Set.Request        as SetSMode
import qualified Generated.Proto.Interpreter.Interpreter.SerializationMode.Set.Update         as SetSMode
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Request                   as SetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Update                    as SetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Request                 as SetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Update                  as SetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Request               as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Update                as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Request              as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Status               as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Request            as WatchPointRemove
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Update             as WatchPointRemove
import           Luna.Interpreter.Proto.CallPointPath                                         ()
import           Luna.Interpreter.Proto.DefPoint                                              ()
import qualified Luna.Interpreter.RPC.Handler.Cache                                           as Cache
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.RPC.Handler.Sync                                            as Sync
import qualified Luna.Interpreter.Session.AST.Executor                                        as Executor
import qualified Luna.Interpreter.Session.AST.WatchPoint                                      as WatchPoint
import qualified Luna.Interpreter.Session.Env                                                 as Env
import           Luna.Interpreter.Session.Session                                             (SessionST)



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


getProjectID :: GetProjectID.Request -> RPC Context SessionST GetProjectID.Status
getProjectID request = do
    projectID <- liftSession Env.getProjectIDMaybe
    return $ GetProjectID.Status request $ fmap encodeP projectID


setProjectID :: SetProjectID.Request -> RPC Context SessionST SetProjectID.Update
setProjectID request@(SetProjectID.Request tprojectID) = do
    liftSession $ Env.setProjectID $ decodeP tprojectID
    Sync.syncLibManager
    Cache.deleteAll tprojectID
    Cache.modifyAll tprojectID
    return $ SetProjectID.Update request


getMainPtr :: GetMainPtr.Request -> RPC Context SessionST GetMainPtr.Status
getMainPtr request = do
    projectID <- liftSession Env.getProjectIDMaybe
    mainPtr   <- liftSession Env.getMainPtrMaybe
    let tmainPtr = (encode .: (,)) <$> projectID <*> mainPtr
    return $ GetMainPtr.Status request tmainPtr


setMainPtr :: SetMainPtr.Request -> RPC Context SessionST SetMainPtr.Update
setMainPtr request@(SetMainPtr.Request tmainPtr) = do
    (projectID, mainPtr) <- decodeE tmainPtr
    Sync.testProjectID projectID
    liftSession $ Env.setMainPtr mainPtr
    return $ SetMainPtr.Update request


run :: Run.Request -> RPC Context SessionST Run.Update
run request = do
    liftIO $ putStrLn "start..."
    liftIO $ Concurrent.threadDelay 3000000
    liftSession Executor.processMain
    liftIO $ putStrLn "stop"
    return $ Run.Update request


watchPointAdd :: WatchPointAdd.Request -> RPC Context SessionST WatchPointAdd.Update
watchPointAdd request@(WatchPointAdd.Request tcallPointPath) = do
    (projectID, callPointPath) <- decodeE tcallPointPath
    Sync.testProjectID projectID
    liftSession $ WatchPoint.add callPointPath
    return $ WatchPointAdd.Update request


watchPointRemove :: WatchPointRemove.Request -> RPC Context SessionST WatchPointRemove.Update
watchPointRemove request@(WatchPointRemove.Request tcallPointPath) = do
    (projectID, callPointPath) <- decodeE tcallPointPath
    Sync.testProjectID projectID
    liftSession $ WatchPoint.delete callPointPath
    return $ WatchPointRemove.Update request


watchPointList :: WatchPointList.Request -> RPC Context SessionST WatchPointList.Status
watchPointList request = do
    list      <- liftSession $ SetForest.toList <$> WatchPoint.all
    projectID <- liftSession Env.getProjectID
    return $ WatchPointList.Status request $ encodeList $ map (projectID,) list


ping :: Ping.Request -> RPC Context SessionST Ping.Status
ping request = do
    logger info "Ping received"
    return $ Ping.Status request


getDefaultSerializationMode :: GetDefaultSMode.Request -> RPC Context SessionST GetDefaultSMode.Status
getDefaultSerializationMode request = do
    mode <- liftSession Env.getDefaultSerializationMode
    return $ GetDefaultSMode.Status request mode


setDefaultSerializationMode :: SetDefaultSMode.Request -> RPC Context SessionST SetDefaultSMode.Update
setDefaultSerializationMode request@(SetDefaultSMode.Request mode) = do
    liftSession $ Env.setDefaultSerializationMode mode
    return $ SetDefaultSMode.Update request


getSerializationMode :: GetSMode.Request -> RPC Context SessionST GetSMode.Status
getSerializationMode request@(GetSMode.Request tcallPointPath) = do
    (projectID, callPointPath) <- decodeE tcallPointPath
    Sync.testProjectID projectID
    mode <- liftSession $ Env.lookupSerializationMode callPointPath
    return $ GetSMode.Status request mode


setSerializationMode :: SetSMode.Request -> RPC Context SessionST SetSMode.Update
setSerializationMode request@(SetSMode.Request tcallPointPath mode) = do
    (projectID, callPointPath) <- decodeE tcallPointPath
    Sync.testProjectID projectID
    liftSession $ Env.insertSerializationMode callPointPath mode
    return $ SetSMode.Update request


deleteSerializationMode :: DeleteSMode.Request -> RPC Context SessionST DeleteSMode.Update
deleteSerializationMode request@(DeleteSMode.Request tcallPointPath) = do
    (projectID, callPointPath) <- decodeE tcallPointPath
    Sync.testProjectID projectID
    liftSession $ Env.deleteSerializationMode callPointPath
    return $ DeleteSMode.Update request
