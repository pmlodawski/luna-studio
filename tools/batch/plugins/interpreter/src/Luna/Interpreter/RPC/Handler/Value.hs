---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Interpreter.RPC.Handler.Value where

import           Data.ByteString.Lazy (ByteString)
import qualified Pipes.Concurrent     as Pipes

import           Flowbox.Bus.Data.Message                              (Message (Message))
import qualified Flowbox.Bus.Data.Message                              as Message
import           Flowbox.Bus.Data.Topic                                (update, (/+))
import           Flowbox.Bus.RPC.RPC                                   (RPC)
import           Flowbox.Prelude                                       hiding (Context)
import           Flowbox.ProjectManager.Context                        (Context)
import           Flowbox.System.Log.Logger                             hiding (error)
import qualified Flowbox.Text.ProtocolBuffers                          as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Interpreter.Value.Request as Value
import qualified Generated.Proto.Interpreter.Interpreter.Value.Update  as Value
import           Luna.Interpreter.Proto.CallPoint                      ()
import           Luna.Interpreter.Proto.CallPointPath                  ()
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.RPC.Topic                            as Topic
import qualified Luna.Interpreter.Session.Cache.Value                  as Value
import           Luna.Interpreter.Session.Data.CallPointPath           (CallPointPath)
import           Luna.Interpreter.Session.Session                      (SessionST)


logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Value"



get :: Value.Request -> RPC Context SessionST Value.Update
get (Value.Request tcallPointPath) = do
    callPointPath <- decodeE tcallPointPath
    result <- liftSession $ Value.getIfReady callPointPath
    return $ Value.Update tcallPointPath result


reportOutputValue :: Pipes.Output (Message, Maybe Message.CorrelationID)
                  -> CallPointPath -> ByteString -> IO ()
reportOutputValue output callPointPath value = do
    let tcallPointPath = encode callPointPath
        response = Value.Update tcallPointPath value
        topic    = Topic.interpreterValueRequest /+ update
        msg      = Message topic $ Proto.messagePut' response
        packet   = (msg, Nothing)
    void $ Pipes.atomically $ Pipes.send output packet
