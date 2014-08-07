---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Interpreter.RPC.Handler.Interpreter where

import           Flowbox.Bus.RPC.RPC                                               (RPC)
import qualified Flowbox.Data.SetForest                                            as SetForest
import           Flowbox.Interpreter.Proto.CallPoint                               ()
import           Flowbox.Interpreter.Proto.CallPointPath                           ()
import qualified Flowbox.Interpreter.Session.AST.Executor                          as Executor
import qualified Flowbox.Interpreter.Session.AST.WatchPoint                        as WatchPoint
import qualified Flowbox.Interpreter.Session.Data.CallPoint                        as CallPoint
import           Flowbox.Interpreter.Session.SessionT                              (SessionT (SessionT))
import           Flowbox.Prelude                                                   hiding (Context)
import           Flowbox.ProjectManager.Context                                    (Context)
import           Flowbox.System.Log.Logger                                         hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request               as Run
import qualified Generated.Proto.Interpreter.Interpreter.Run.Update                as Run
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Request    as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Add.Update     as WatchPointAdd
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Request   as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.List.Status    as WatchPointList
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Request as WatchPointRemove
import qualified Generated.Proto.Interpreter.Interpreter.WatchPoint.Remove.Update  as WatchPointRemove



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.Interpreter"


run :: Run.Request -> RPC Context SessionT Run.Update
run request = do
    lift2 $ SessionT Executor.processMain
    return $ Run.Update request


watchPointAdd :: WatchPointAdd.Request -> RPC Context SessionT WatchPointAdd.Update
watchPointAdd request@(WatchPointAdd.Request tcallPointPath) = do
    callPointPath <- decodeE tcallPointPath
    lift2 $ SessionT $ WatchPoint.add callPointPath
    return $ WatchPointAdd.Update request


watchPointRemove :: WatchPointRemove.Request -> RPC Context SessionT WatchPointRemove.Update
watchPointRemove request@(WatchPointRemove.Request tcallPointPath) = do
    callPointPath <- decodeE tcallPointPath
    lift2 $ SessionT $ WatchPoint.delete callPointPath
    return $ WatchPointRemove.Update request


watchPointList :: WatchPointList.Request -> RPC Context SessionT WatchPointList.Status
watchPointList request = do
    list <- lift2 $ SessionT $ SetForest.toList <$> WatchPoint.all
    return $ WatchPointList.Status request $ encodeList list

