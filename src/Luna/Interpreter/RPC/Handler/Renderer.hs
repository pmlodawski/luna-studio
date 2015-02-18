---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.RPC.Handler.Renderer where

import           Flowbox.Bus.RPC.RPC                              (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                  hiding (Context)
import           Flowbox.ProjectManager.Context                   (Context)
import           Flowbox.System.Log.Logger                        hiding (error)
import qualified Generated.Proto.Renderer.Renderer.Render.Request as Render
import qualified Generated.Proto.Renderer.Renderer.Render.Update  as Render
import           Luna.Interpreter.RPC.Handler.Lift                (liftSession)
import           Luna.Interpreter.Session.Memory.Manager          (MemoryManager)
import           Luna.Interpreter.Session.Session                 (SessionST)
import           Luna.Renderer.Proto.FrameRange                   ()
import qualified Luna.Renderer.Renderer                           as Renderer



logger :: LoggerIO
logger = getLoggerIO $moduleName


render :: MemoryManager mm
       => Render.Request -> RPC Context (SessionST mm) Render.Update
render request@(Render.Request ranges filePattern _) = do
    liftSession $ Renderer.render (decodeP ranges) (decodeP filePattern)
    logger info "Render not implemented"
    return $ Render.Update request
