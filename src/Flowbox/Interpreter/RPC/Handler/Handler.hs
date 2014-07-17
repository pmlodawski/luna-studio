---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Interpreter.RPC.Handler.Handler where

import qualified Data.Map as Map
import           Pipes


import           Flowbox.Bus.BusT                            (BusT (BusT))
import           Flowbox.Bus.Data.Message                    (Message)
import qualified Flowbox.Bus.Data.Message                    as Message
import           Flowbox.Bus.Data.Topic                      (Topic)
import qualified Flowbox.Bus.Data.Topic                      as Topic
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import qualified Flowbox.Bus.RPC.Pipes                       as RPC
import           Flowbox.Bus.RPC.RPC                         (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import           Flowbox.Control.Error
import           Flowbox.Interpreter.Context                 (Context)
import qualified Flowbox.Interpreter.Context                 as Context
import qualified Flowbox.Interpreter.RPC.Handler.Interpreter as InterpreterHandler
import qualified Flowbox.Interpreter.RPC.Topic               as Topic
import qualified Flowbox.Interpreter.Session.Env             as Env
import qualified Flowbox.Interpreter.Session.Session         as Session
import           Flowbox.Interpreter.Session.SessionT        (SessionT)
import qualified Flowbox.Interpreter.Session.SessionT        as SessionT
import           Flowbox.Prelude                             hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.Handler"


--handlerMap :: Context -> HandlerMap
--handlerMap ctx callback = HandlerMap.fromList
--    [ (Topic.interpreterWatchPointAdd    , call Topic.update $ fmap Context.watchPointAddUpdate . handleCommand ctx . Context.WatchPointAddRequest)
--    --, (Topic.interpreterWatchPointRemove , call Topic.update $ undefined)
--    --, (Topic.interpreterWatchPointList   , call Topic.status $ undefined)
--    --, (Topic.interpreterInvalidateCall   , call Topic.update $ undefined)
--    --, (Topic.interpreterInvalidateDef    , call Topic.update $ undefined)
--    --, (Topic.interpreterInvalidateNode   , call Topic.update $ undefined)
--    --, (Topic.interpreterRun              , call Topic.update $ undefined)
--    ]
--    where
--        call :: (Proto.Serializable args, Proto.Serializable result)
--             => String -> (args -> RPC result) -> IO [Message]
--        call type_ = callback type_ . Processor.singleResult


--handleCommand :: Context -> Context.Request -> RPC Context.Result
--handleCommand ctx request = do
--    undefined



busHandle :: Pipe (Message, Message.CorrelationID) (Message, Message.CorrelationID) BusT ()
busHandle = do
    let env = Env.mk undefined undefined
    hoist (\a -> liftIO $ eitherToM =<< (Session.run env $ SessionT.runSessionT a)) interpreterHandle


interpreterHandle :: Pipe (Message, Message.CorrelationID) (Message, Message.CorrelationID) SessionT ()
interpreterHandle = do
    (message, crl) <- await
    --let topic = message ^. Message.topic
    --result <- safeLiftIO $ do
    --        undefined
    --response <- case result of
    --    Left err               -> do logger error err
    --                                 return $ Just $ Message.mkError topic err
    --    Right (type_, content) -> map Message.mkResponse topic type_ content
    --mapM_ (\r -> yield (r, crl)) response
    undefined

--parseRequest :: Pipe (Message, Message.CorrelationID) (Context.Request, Message.CorrelationID) BusT ()
--parseRequest = do
--    (message, crl) <- await
--    let mkCommand wrapper = case Proto.messageGet' $ message ^. Message.message of
--            Left err   -> Context.Invalid $ "Cannot parse message : " ++ err
--            Right args -> wrapper args
--        commands = Map.fromList
--                    [ (Topic.interpreterWatchPointAdd   , mkCommand Context.WatchPointAddRequest)
--                    , (Topic.interpreterWatchPointRemove, mkCommand Context.WatchPointRemoveRequest)
--                    , (Topic.interpreterWatchPointList  , mkCommand Context.WatchPointListRequest)
--                    , (Topic.interpreterInvalidateCall  , mkCommand Context.InvalidateCallRequest)
--                    , (Topic.interpreterInvalidateDef   , mkCommand Context.InvalidateDefRequest)
--                    , (Topic.interpreterInvalidateNode  , mkCommand Context.InvalidateNodeRequest)
--                    , (Topic.interpreterRun             , mkCommand Context.RunRequest)
--                    ]
--        topic   = message ^. Message.topic
--        request = case Map.lookup topic commands of
--                    Just request -> request
--                    Nothing      -> Context.Invalid $ "Unknown topic: " ++ show topic
--    yield (request, crl)


--prepareResponse :: Pipe (Context.Response, Message.CorrelationID) (Message, Message.CorrelationID) BusT ()
--prepareResponse = do
--    (response, crl) <- await

--    let message = Message $ uncurry $ case response of
--            Context.WatchPointAddUpdate -> Topic.interpreterWatchPointAdd
--            Context.WatchPointRemoveUpdate
--            Context.WatchPointListStatus
--            Context.InvalidateCallUpdate
--            Context.InvalidateDefUpdate
--            Context.InvalidateNodeUpdate
--            Context.RunUpdate
--    yield (message, crl)
--    undefined




--handler :: Pipe (Message, Message.CorrelationID) (Message, Message.CorrelationID) BusT ()
--handler = do
--    (request, crl) <- await
--    case request ^. Message.topic of

--    yield (response, crl)

