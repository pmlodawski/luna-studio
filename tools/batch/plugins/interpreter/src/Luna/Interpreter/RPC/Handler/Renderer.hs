---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Luna.Interpreter.RPC.Handler.Renderer where

import qualified Data.Sequence    as Sequence
import qualified Pipes.Concurrent as Pipes

import           Flowbox.Bus.Data.Flag                                (Flag)
import qualified Flowbox.Bus.Data.Flag                                as Flag
import           Flowbox.Bus.Data.Message                             (Message (Message))
import qualified Flowbox.Bus.Data.Message                             as Message
import           Flowbox.Bus.Data.Topic                               ((/+))
import           Flowbox.Bus.RPC.RPC                                  (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                      hiding (Context)
import           Flowbox.ProjectManager.Context                       (Context)
import           Flowbox.System.Log.Logger                            hiding (error)
import qualified Flowbox.Text.ProtocolBuffers                         as Proto
import qualified Generated.Proto.Renderer.Renderer.Render.Progress    as Gen
import qualified Generated.Proto.Renderer.Renderer.Render.Request     as Render
import qualified Generated.Proto.Renderer.Renderer.Render.Update      as Render
import qualified Generated.Proto.Renderer.Renderer.RenderNode.Request as RenderNode
import qualified Generated.Proto.Renderer.Renderer.RenderNode.Update  as RenderNode
import           Luna.Interpreter.RPC.Handler.Lift                    (liftSession)
import qualified Luna.Interpreter.RPC.Handler.Sync                    as Sync
import qualified Luna.Interpreter.RPC.Topic                           as Topic
import qualified Luna.Interpreter.Session.Env                         as Env
import           Luna.Interpreter.Session.Memory.Manager              (MemoryManager)
import           Luna.Interpreter.Session.Session                     (SessionST)
import           Luna.Renderer.Proto.FrameCompileError                ()
import           Luna.Renderer.Proto.FrameRange                       ()
import qualified Luna.Renderer.Renderer                               as Renderer



logger :: LoggerIO
logger = getLoggerIO $moduleName


render :: MemoryManager mm
       => Renderer.ProgressReporter -> Render.Request -> RPC Context (SessionST mm) Render.Update
render progress request@(Render.Request ranges _) = do
    results   <- liftSession $ Renderer.render (decodeP ranges) progress
    projectID <- liftSession Env.getProjectID
    return $ Render.Update request $ Sequence.fromList $ map (encodeP . (projectID,)) results



renderNode :: MemoryManager mm
           => Renderer.ProgressReporter -> RenderNode.Request -> RPC Context (SessionST mm) RenderNode.Update
renderNode progress request@(RenderNode.Request tcallPointPath ranges _) = do
    let (projectID, callPointPath) = decodeP tcallPointPath
    Sync.testProjectID projectID
    liftSession $ Renderer.renderNode callPointPath (decodeP ranges) progress
    return $ RenderNode.Update request


reportProgress :: Message.CorrelationID
               -> Pipes.Output (Message, Message.CorrelationID, Flag)
               -> Renderer.ProgressReporter
reportProgress crl output current total = do
    let response = Gen.Progress (encodeP current) (encodeP total)
        topic    = Topic.rendererRenderRequest /+ "progress"
        msg      = Message topic $ Proto.messagePut' response
        packet   = (msg, crl, Flag.Disable)
    logger debug $ "Rendering progress " <> show current <> " / " <> show total
    void $ Pipes.atomically $ Pipes.send output packet
