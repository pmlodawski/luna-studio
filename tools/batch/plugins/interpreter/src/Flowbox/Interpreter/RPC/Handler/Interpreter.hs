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
import qualified Flowbox.Interpreter.Session.Cache.Invalidate                      as Invalidate
import qualified Flowbox.Interpreter.Session.Data.CallPoint                        as CallPoint
import           Flowbox.Interpreter.Session.SessionT                              (SessionT (SessionT))
import           Flowbox.Prelude                                                   hiding (Context)
import           Flowbox.ProjectManager.Context                                    (Context)
import           Flowbox.System.Log.Logger                                         hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Interpreter.Invalidate.Call.Request   as InvalidateCall
import qualified Generated.Proto.Interpreter.Interpreter.Invalidate.Call.Update    as InvalidateCall
import qualified Generated.Proto.Interpreter.Interpreter.Invalidate.Def.Request    as InvalidateDef
import qualified Generated.Proto.Interpreter.Interpreter.Invalidate.Def.Update     as InvalidateDef
import qualified Generated.Proto.Interpreter.Interpreter.Invalidate.Node.Request   as InvalidateNode
import qualified Generated.Proto.Interpreter.Interpreter.Invalidate.Node.Update    as InvalidateNode
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


invalidateCall :: InvalidateCall.Request -> RPC Context SessionT InvalidateCall.Update
invalidateCall request@(InvalidateCall.Request tcallPointPath) = do
    callPointPath <- decodeE tcallPointPath
    lift2 $ SessionT $ Invalidate.invalidate callPointPath
    return $ InvalidateCall.Update request


invalidateDef :: InvalidateDef.Request -> RPC Context SessionT InvalidateDef.Update
invalidateDef request@(InvalidateDef.Request tlibraryID tdefID) = do
    let libraryID = decodeP tlibraryID
        defID     = decodeP tdefID
    lift2 $ SessionT $ Invalidate.invalidateDef libraryID defID
    return $ InvalidateDef.Update request


invalidateNode :: InvalidateNode.Request -> RPC Context SessionT InvalidateNode.Update
invalidateNode request@(InvalidateNode.Request tcallPoint) = do
    callPoint <- decodeE tcallPoint
    lift2 $ SessionT $ Invalidate.invalidateNode (callPoint ^. CallPoint.libraryID) (callPoint ^. CallPoint.nodeID)
    return $ InvalidateNode.Update request


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

