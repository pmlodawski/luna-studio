---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.Interpreter.Context where

import qualified Pipes.Concurrent as Pipes

import           Flowbox.Prelude                                                   hiding (Context)
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




data Request = WatchPointAddRequest    WatchPointAdd.Request
             | WatchPointRemoveRequest WatchPointRemove.Request
             | WatchPointListRequest   WatchPointList.Request
             | InvalidateCallRequest   InvalidateCall.Request
             | InvalidateDefRequest    InvalidateDef.Request
             | InvalidateNodeRequest   InvalidateNode.Request
             | RunRequest              Run.Request
             | Invalid                 String


data Response = WatchPointAddUpdate    { watchPointAddUpdate :: WatchPointAdd.Update }
              | WatchPointRemoveUpdate { watchPointRemoveUpdate :: WatchPointRemove.Update }
              | WatchPointListStatus   { watchPointListStatus :: WatchPointList.Status }
              | InvalidateCallUpdate   { invalidateCallUpdate :: InvalidateCall.Update }
              | InvalidateDefUpdate    { invalidateDefUpdate :: InvalidateDef.Update }
              | InvalidateNodeUpdate   { invalidateNodeUpdate :: InvalidateNode.Update }
              | RunUpdate              { runUpdate :: Run.Update }


data Context = Context { _requestOutput :: Pipes.Output Request
                       , _requestInput  :: Pipes.Input  Request
                       , _resultOutput  :: Pipes.Output Response
                       , _resultInput   :: Pipes.Input  Response
                       }


makeLenses(''Context)


mk :: IO Context
mk = do
    (reqOutput, reqInput) <- Pipes.spawn Pipes.Unbounded
    (resOutput, resInput) <- Pipes.spawn Pipes.Unbounded
    return $ Context reqOutput reqInput resOutput resInput
