---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.Interpreter.RPC.Handler.Abort where

import qualified Control.Monad.Catch as Catch

import qualified Flowbox.Control.Concurrent                   as Concurrent
import           Flowbox.Prelude                              hiding (Context, error)
import           Flowbox.System.Log.Logger
import           Luna.Interpreter.Session.Data.AbortException (AbortException (AbortException))
import qualified Luna.Interpreter.Session.Env                 as Env



logger :: LoggerIO
logger = getLoggerIO $moduleName


handleAbort :: (Catch.MonadCatch m, MonadIO m) => m () -> m ()
handleAbort =
    flip Catch.catch $ \AbortException -> return () --logger info "Nothing to abort"


abort :: Env.FragileMVar -> Concurrent.ThreadId -> IO ()
abort fm threadId = Concurrent.withMVar fm $ const $
    --logger warning "Abort requested."
    Concurrent.throwTo threadId AbortException

