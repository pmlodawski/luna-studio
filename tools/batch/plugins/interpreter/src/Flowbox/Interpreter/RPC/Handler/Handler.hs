---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Interpreter.RPC.Handler.Handler where

import           Control.Monad    (forever)
import           Pipes            (lift, (>->))
import qualified Pipes
import qualified Pipes.Concurrent as Pipes

import           Flowbox.Bus.Data.Message                    (Message)
import qualified Flowbox.Bus.Data.Message                    as Message
import qualified Flowbox.Bus.Data.Topic                      as Topic
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import qualified Flowbox.Interpreter.RPC.Handler.ASTWatch    as ASTWatch
import qualified Flowbox.Interpreter.RPC.Handler.Interpreter as Interpreter
import qualified Flowbox.Interpreter.RPC.Topic               as Topic
import           Flowbox.Interpreter.Session.Error           (Error)
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
    --, (Topic.projectLibraryAstGetRequest , call0 ASTWatch.test)
    , ("project.store.status"            , call0 ASTWatch.test2)
    , ("dummy"                           , call0 ASTWatch.test2)
    ]
    where
        call1 type_ = callback type_ . Processor.singleResult
        call0       = callback ""    . Processor.noResult


interpret :: Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Message.CorrelationID)
                        SessionT ()
interpret = forever $ do
    (message, crl) <- Pipes.await
    results <- lift $ Processor.processLifted handlerMap message
    mapM_ (\r -> Pipes.yield (r, crl)) results



run :: (Pipes.Input  (Message, Message.CorrelationID),
        Pipes.Output (Message, Message.CorrelationID))
    -> IO (Either Error ())
run (input, output) = Session.run def $ SessionT.runSessionT $ do
    Pipes.runEffect $ Pipes.fromInput input
                  >-> interpret
                  >-> Pipes.toOutput output
