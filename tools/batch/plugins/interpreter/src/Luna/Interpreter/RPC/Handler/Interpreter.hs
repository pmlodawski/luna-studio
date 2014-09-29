---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
module Luna.Interpreter.RPC.Handler.Interpreter where

import           Flowbox.Bus.RPC.RPC                                               (RPC)
import qualified Flowbox.Data.SetForest                                            as SetForest
import           Flowbox.Prelude                                                   hiding (Context)
import           Flowbox.ProjectManager.Context                                    (Context)
import           Flowbox.System.Log.Logger                                         hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Interpreter.GetMainPtr.Request        as GetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.GetMainPtr.Status         as GetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.GetProjectID.Request      as GetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.GetProjectID.Status       as GetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.Ping.Request              as Ping
import qualified Generated.Proto.Interpreter.Interpreter.Ping.Status               as Ping
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request               as Run
import qualified Generated.Proto.Interpreter.Interpreter.Run.Update                as Run
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Request        as SetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Update         as SetMainPtr
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Request      as SetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Update       as SetProjectID
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Request    as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Update     as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Request   as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Status    as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Request as WatchPointRemove
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Update  as WatchPointRemove
import           Luna.Interpreter.Proto.CallPointPath                              ()
import           Luna.Interpreter.Proto.DefPoint                                   ()
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.RPC.Handler.Modify                               as Modify
import qualified Luna.Interpreter.RPC.Handler.Sync                                 as Sync
import qualified Luna.Interpreter.Session.AST.Executor                             as Executor
import qualified Luna.Interpreter.Session.AST.WatchPoint                           as WatchPoint
import           Luna.Interpreter.Session.Session                                  (SessionST)
import qualified Luna.Interpreter.Session.Session                                  as Session



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Interpreter"


getProjectID :: GetProjectID.Request -> RPC Context SessionST GetProjectID.Status
getProjectID request = do
    projectID <- liftSession Session.getProjectIDMaybe
    return $ GetProjectID.Status request $ fmap encodeP projectID


setProjectID :: SetProjectID.Request -> RPC Context SessionST SetProjectID.Update
setProjectID request@(SetProjectID.Request tprojectID) = do
    liftSession $ Session.setProjectID $ decodeP tprojectID
    Sync.syncLibManager
    Modify.modifyAll tprojectID
    return $ SetProjectID.Update request


getMainPtr :: GetMainPtr.Request -> RPC Context SessionST GetMainPtr.Status
getMainPtr request = do
    projectID <- liftSession Session.getProjectIDMaybe
    mainPtr   <- liftSession Session.getMainPtrMaybe
    let tmainPtr = (encode .: (,)) <$> projectID <*> mainPtr
    return $ GetMainPtr.Status request tmainPtr


setMainPtr :: SetMainPtr.Request -> RPC Context SessionST SetMainPtr.Update
setMainPtr request@(SetMainPtr.Request tmainPtr) = do
    (projectID, mainPtr) <- decodeE tmainPtr
    Sync.testProjectID projectID
    liftSession $ Session.setMainPtr mainPtr
    return $ SetMainPtr.Update request


run :: Run.Request -> RPC Context SessionST Run.Update
run request = do
    liftSession Executor.processMain
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
    projectID <- liftSession Session.getProjectID
    return $ WatchPointList.Status request $ encodeList $ map (projectID,) list


ping :: Ping.Request -> RPC Context SessionST Ping.Status
ping request = do
    logger info "Ping received"
    return $ Ping.Status request

