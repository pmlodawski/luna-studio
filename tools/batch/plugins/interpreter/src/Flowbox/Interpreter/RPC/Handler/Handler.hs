---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Interpreter.RPC.Handler.Handler where

import Control.Monad (forever)
import Pipes

import           Flowbox.Bus.BusT                            (BusT)
import           Flowbox.Bus.Data.Message                    (Message)
import qualified Flowbox.Bus.Data.Message                    as Message
import qualified Flowbox.Bus.Data.Topic                      as Topic
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import           Flowbox.Control.Error
import qualified Flowbox.Interpreter.RPC.Handler.ASTWatch    as ASTWatch
import qualified Flowbox.Interpreter.RPC.Handler.Interpreter as Interpreter
import qualified Flowbox.Interpreter.RPC.Topic               as Topic
import qualified Flowbox.Interpreter.Session.Session         as Session
import           Flowbox.Interpreter.Session.SessionT        (SessionT)
import qualified Flowbox.Interpreter.Session.SessionT        as SessionT
import           Flowbox.Prelude                             hiding (Context, error)
import qualified Flowbox.ProjectManager.Topic                as Topic
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.Handler"


handlerMap :: HandlerMap SessionT
handlerMap callback = HandlerMap.fromList
    [ (Topic.interpreterInvalidateCall   , call1 Topic.update $ Interpreter.invalidateCall)
    , (Topic.interpreterInvalidateDef    , call1 Topic.update $ Interpreter.invalidateDef)
    , (Topic.interpreterInvalidateNode   , call1 Topic.update $ Interpreter.invalidateNode)
    , (Topic.interpreterRun              , call1 Topic.update $ Interpreter.run)
    , (Topic.interpreterWatchPointAdd    , call1 Topic.update $ Interpreter.watchPointAdd)
    , (Topic.interpreterWatchPointRemove , call1 Topic.update $ Interpreter.watchPointRemove)
    , (Topic.interpreterWatchPointList   , call1 Topic.status $ Interpreter.watchPointList)
    , (Topic.projectLibraryAstGetRequest , call0 ASTWatch.test)
    --, (Topic.projectStoreRequest         , call0 ASTWatch.test)
    ]
    where
        call1 type_ = callback type_ . Processor.singleResult
        call0       = callback ""    . Processor.noResult


handle :: Pipe (Message, Message.CorrelationID)
               (Message, Message.CorrelationID)
               BusT ()
handle = hoist
    (\a -> liftIO $ eitherToM =<< (Session.run def $ SessionT.runSessionT a))
    interpreterHandle


interpreterHandle :: Pipe (Message, Message.CorrelationID)
                          (Message, Message.CorrelationID)
                          SessionT ()
interpreterHandle = forever $ do
    (message, crl) <- await
    results <- lift $ Processor.processLifted handlerMap message
    mapM_ (\r -> yield (r, crl)) results
