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

import           Control.Monad                      (forever)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import qualified Pipes

import           Flowbox.Bus.Data.Message           (Message)
import qualified Flowbox.Bus.Data.Message           as Message
import           Flowbox.Bus.Data.Prefix            (Prefix)
import qualified Flowbox.Bus.Data.Prefix            as Prefix
import           Flowbox.Bus.Data.Topic             (Topic)
import qualified Flowbox.Control.Concurrent         as Concurrent
import           Flowbox.Prelude                    hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Luna.Interpreter.RPC.Handler.Abort as Abort
import           Luna.Interpreter.RPC.QueueInfo     (QueueInfo)
import qualified Luna.Interpreter.RPC.QueueInfo     as QueueInfo
import qualified Luna.Interpreter.RPC.Topic         as Topic
import qualified Luna.Interpreter.Session.Env       as Env



logger :: LoggerIO
logger = getLoggerIO $moduleName


type ProprocessorAction = QueueInfo -> Message.CorrelationID
                       -> Env.FragileMVar -> Concurrent.ThreadId -> IO ()


preprocess :: Prefix -> QueueInfo -> Env.FragileMVar -> Concurrent.ThreadId
           -> Pipes.Pipe (Message, Message.CorrelationID)
                         (Message, Message.CorrelationID)
                         IO ()
preprocess prefix queueInfo fm threadId = forever $ do
    packet <- Pipes.await
    let topic  = packet ^. _1 . Message.topic
        crl    = packet ^. _2
        action = fromMaybe defaultAction
               $ Map.lookup topic
               $ preprocessorActions prefix
    liftIO $ action queueInfo crl fm threadId
    Pipes.yield packet


preprocessorActions :: Prefix -> Map Topic ProprocessorAction
preprocessorActions prefix = Map.fromList $ Prefix.prefixifyTopics prefix
    [ (Topic.interpreterAbortRequest, \_ _ fm threadId -> Abort.abort fm threadId)
    , (Topic.interpreterRunRequest  , QueueInfo.overrideRun)
    ]


defaultAction :: ProprocessorAction
defaultAction = const (const (const (void . return)))
