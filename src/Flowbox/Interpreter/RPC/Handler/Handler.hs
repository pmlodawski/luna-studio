---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Interpreter.RPC.Handler.Handler where

import           Flowbox.Bus.Data.Message                    (Message)
import           Flowbox.Bus.Data.Topic                      (Topic)
import qualified Flowbox.Bus.Data.Topic                      as Topic
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import           Flowbox.Bus.RPC.RPC                         (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import           Flowbox.Interpreter.Context                 (ContextRef)
import qualified Flowbox.Interpreter.RPC.Handler.Interpreter as InterpreterHandler
import qualified Flowbox.Interpreter.RPC.Topic               as Topic
import           Flowbox.Prelude                             hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.Handler"


handlerMap :: ContextRef -> HandlerMap
handlerMap ctx callback = HandlerMap.fromList
    [ (Topic.interpreterWatchPointAdd    , call Topic.update $ InterpreterHandler.add     ctx)
    , (Topic.interpreterWatchPointRemove , call Topic.update $ InterpreterHandler.remove  ctx)
    , (Topic.interpreterWatchPointList   , call Topic.status $ InterpreterHandler.list    ctx)
    , (Topic.interpreterInvalidateCall   , call Topic.update $ InterpreterHandler.lookup  ctx)
    , (Topic.interpreterInvalidateDef    , call Topic.update $ InterpreterHandler.start   ctx)
    , (Topic.interpreterInvalidateNode   , call Topic.update $ InterpreterHandler.stop    ctx)
    , (Topic.interpreterRun              , call Topic.update $ InterpreterHandler.restart ctx)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC result) -> IO [Message]
        call type_ = callback type_ . Processor.singleResult
