---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.RPC.Handler.Renderer where

import           Flowbox.Bus.RPC.RPC                                 (RPC)
import           Flowbox.Prelude                                     hiding (Context)
import           Flowbox.ProjectManager.Context                      (Context)
import           Flowbox.System.Log.Logger                           hiding (error)
import qualified Generated.Proto.Interpreter.Renderer.Render.Request as Render
import qualified Generated.Proto.Interpreter.Renderer.Render.Update  as Render
import           Luna.Interpreter.Session.Session                    (SessionST)



logger :: LoggerIO
logger = getLoggerIO $moduleName


render :: Render.Request -> RPC Context (SessionST mm) Render.Update
render request@(Render.Request {}) = do
    logger info "Render not implemented"
    return $ Render.Update request
