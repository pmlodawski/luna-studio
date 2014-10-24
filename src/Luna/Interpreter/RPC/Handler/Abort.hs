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

import           Control.Exception.Base (Exception)
import qualified Control.Monad.Catch    as Catch
import           Data.Typeable          (Typeable)
import           Pipes                  (MonadIO)

import qualified Flowbox.Control.Concurrent as Concurrent
import           Flowbox.Prelude            hiding (Context, error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)



data AbortException = AbortException
    deriving (Show, Typeable)

instance Exception AbortException


handleAbort :: (Catch.MonadCatch m, MonadIO m) => m () -> m ()
handleAbort =
    flip Catch.catch $ \AbortException -> logger info "Nothing to abort"


abort :: Concurrent.ThreadId -> IO ()
abort threadId = do
    logger warning "Abort requested."
    Concurrent.throwTo threadId AbortException

