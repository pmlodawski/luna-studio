---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.RPC.Handler.Renderer where

import qualified Pipes.Concurrent as Pipes

import           Flowbox.Bus.Data.Flag                            (Flag)
import           Flowbox.Bus.Data.Message                         (Message (Message))
import qualified Flowbox.Bus.Data.Message                         as Message
import           Flowbox.Bus.RPC.RPC                              (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                  hiding (Context)
import           Flowbox.ProjectManager.Context                   (Context)
import           Flowbox.System.Log.Logger                        hiding (error)
import qualified Generated.Proto.Renderer.Renderer.Render.Request as Render
import qualified Generated.Proto.Renderer.Renderer.Render.Update  as Render
import qualified Luna.Interpreter.RPC.Topic                            as Topic
import           Luna.Interpreter.RPC.Handler.Lift                (liftSession)
import           Flowbox.Bus.Data.Topic                                ((/+))
import qualified Flowbox.Text.ProtocolBuffers                          as Proto
import           Luna.Interpreter.Session.Memory.Manager          (MemoryManager)
import qualified Flowbox.Bus.Data.Flag                                 as Flag
import           Luna.Interpreter.Session.Session                 (SessionST)
import           Luna.Renderer.Proto.FrameRange                   ()
import qualified Luna.Renderer.Renderer                           as Renderer
import qualified Generated.Proto.Renderer.Renderer.Render.Progress as Gen



logger :: LoggerIO
logger = getLoggerIO $moduleName


render :: MemoryManager mm
       => Render.Request -> RPC Context (SessionST mm) Render.Update
render request@(Render.Request ranges _) = do
    liftSession $ Renderer.render (decodeP ranges)
    logger info "Render not implemented"
    return $ Render.Update request


reportProgress :: Message.CorrelationID
               -> Pipes.Output (Message, Message.CorrelationID, Flag)
               -> Int -> Int -> IO ()
reportProgress crl output current total = do
    let response = Gen.Progress (encodeP current) (encodeP total)
        topic    = Topic.rendererRenderRequest /+ "progress"
        msg      = Message topic $ Proto.messagePut' response
        packet   = (msg, crl, Flag.Disable)
    logger debug $ "Rendering progress " <> show current <> " / " <> show total
    void $ Pipes.atomically $ Pipes.send output packet
