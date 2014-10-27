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

module Luna.Interpreter.RPC.Handler.Preprocess where

import           Control.Monad (forever)
import           Pipes         (liftIO)
import qualified Pipes

import           Flowbox.Bus.Data.Message           (Message)
import qualified Flowbox.Bus.Data.Message           as Message
import           Flowbox.Bus.Data.Prefix            (Prefix)
import qualified Flowbox.Bus.Data.Prefix            as Prefix
import qualified Flowbox.Control.Concurrent         as Concurrent
import           Flowbox.Prelude                    hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Luna.Interpreter.RPC.Handler.Abort as Abort
import qualified Luna.Interpreter.RPC.Topic         as Topic



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


preprocess :: Prefix -> Concurrent.ThreadId
           -> Pipes.Pipe (Message, Message.CorrelationID)
                         (Message, Message.CorrelationID)
                         IO ()
preprocess prefix threadId = forever $ do
    packet <- Pipes.await
    when ((packet ^. _1 . Message.topic)
            == Prefix.prefixify prefix Topic.interpreterAbortRequest) $
        liftIO $ Abort.abort threadId
    Pipes.yield packet


