---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.RPC.Handler.Value where

import           Data.IORef       (IORef)
import qualified Data.IORef       as IORef
import qualified Data.Sequence    as Sequence
import qualified Pipes.Concurrent as Pipes

import           Flowbox.Bus.Data.Flag                                 (Flag)
import qualified Flowbox.Bus.Data.Flag                                 as Flag
import           Flowbox.Bus.Data.Message                              (Message (Message))
import qualified Flowbox.Bus.Data.Message                              as Message
import           Flowbox.Bus.Data.Topic                                (update, (/+))
import           Flowbox.Bus.RPC.RPC                                   (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                       hiding (Context)
import           Flowbox.ProjectManager.Context                        (Context)
import           Flowbox.System.Log.Logger                             hiding (error)
import qualified Flowbox.Text.ProtocolBuffers                          as Proto
import qualified Generated.Proto.Interpreter.Interpreter.Value.Request as Value
import qualified Generated.Proto.Interpreter.Interpreter.Value.Update  as Value
import           Luna.Interpreter.Proto.CallPoint                      ()
import           Luna.Interpreter.Proto.CallPointPath                  ()
import           Luna.Interpreter.Proto.Status                         ()
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.RPC.Handler.Sync                     as Sync
import qualified Luna.Interpreter.RPC.Topic                            as Topic
import qualified Luna.Interpreter.Session.Cache.Value                  as Value
import           Luna.Interpreter.Session.Env.Env                      (ResultCallBack)
import           Luna.Interpreter.Session.Session                      (SessionST)



logger :: LoggerIO
logger = getLoggerIO $moduleName


get :: Value.Request -> RPC Context (SessionST mm) Value.Update
get (Value.Request tcallPointPath time) = do
    let (projectID, callPointPath) = decodeP tcallPointPath
    Sync.testProjectID projectID
    (status, bytes) <- liftSession $ Value.getWithStatus callPointPath time
    return $ Value.Update tcallPointPath (encodeP status) (Sequence.fromList bytes) time


reportOutputValue :: IORef Message.CorrelationID
                  -> Pipes.Output (Message, Message.CorrelationID, Flag)
                  -> ResultCallBack
reportOutputValue crlRef output projectID callPointPath values time = do
    crl <- IORef.readIORef crlRef
    let tcallPointPath = encodeP (projectID, callPointPath)
        response = Value.Update tcallPointPath (encodeP Value.Ready) (Sequence.fromList values) time
        topic    = Topic.interpreterValueRequest /+ update
        msg      = Message topic $ Proto.messagePut' response
        packet   = (msg, crl, Flag.Disable)
    logger debug $ "Reporting " ++ show (projectID, callPointPath)
    void $ Pipes.atomically $ Pipes.send output packet
