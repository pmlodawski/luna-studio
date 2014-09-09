---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Interpreter.RPC.Handler.Interpreter where

import           Flowbox.Bus.RPC.RPC                                               (RPC)
import qualified Flowbox.Data.SetForest                                            as SetForest
import           Flowbox.Prelude                                                   hiding (Context)
import           Flowbox.ProjectManager.Context                                    (Context)
import           Flowbox.System.Log.Logger                                         hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Interpreter.Ping.Request              as Ping
import qualified Generated.Proto.Interpreter.Interpreter.Ping.Status               as Ping
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request               as Run
import qualified Generated.Proto.Interpreter.Interpreter.Run.Update                as Run
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Request    as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Update     as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Request   as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Status    as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Request as WatchPointRemove
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Update  as WatchPointRemove
import           Luna.Interpreter.Proto.CallPointPath                              ()
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.Session.AST.Executor                             as Executor
import qualified Luna.Interpreter.Session.AST.WatchPoint                           as WatchPoint
import           Luna.Interpreter.Session.Session                                  (SessionST)



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Interpreter"


run :: Run.Request -> RPC Context SessionST Run.Update
run request = do
    liftSession Executor.processMain
    return $ Run.Update request


watchPointAdd :: WatchPointAdd.Request -> RPC Context SessionST WatchPointAdd.Update
watchPointAdd request@(WatchPointAdd.Request tcallPointPath) = do
    callPointPath <- decodeE tcallPointPath
    liftSession $ WatchPoint.add callPointPath
    return $ WatchPointAdd.Update request


watchPointRemove :: WatchPointRemove.Request -> RPC Context SessionST WatchPointRemove.Update
watchPointRemove request@(WatchPointRemove.Request tcallPointPath) = do
    callPointPath <- decodeE tcallPointPath
    liftSession $ WatchPoint.delete callPointPath
    return $ WatchPointRemove.Update request


watchPointList :: WatchPointList.Request -> RPC Context SessionST WatchPointList.Status
watchPointList request = do
    list <- liftSession $ SetForest.toList <$> WatchPoint.all
    return $ WatchPointList.Status request $ encodeList list


ping :: Ping.Request -> RPC Context SessionST Ping.Status
ping request = do
    logger info "Ping received"
    return $ Ping.Status request
