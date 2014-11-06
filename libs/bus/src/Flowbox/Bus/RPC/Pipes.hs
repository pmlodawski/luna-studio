---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.RPC.Pipes where

import           Pipes            ((>->))
import qualified Pipes
import qualified Pipes.Concurrent as Pipes

import           Control.Monad                 (forever)
import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.BusT              (BusT (BusT))
import qualified Flowbox.Bus.BusT              as BusT
import           Flowbox.Bus.Data.Flag         (Flag)
import           Flowbox.Bus.Data.Message      (Message)
import qualified Flowbox.Bus.Data.Message      as Message
import qualified Flowbox.Bus.Data.MessageFrame as MessageFrame
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Bus.RPC.HandlerMap    (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap    as HandlerMap
import           Flowbox.Control.Concurrent    (forkIO_)
import           Flowbox.Control.Error
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


produce :: Pipes.Producer (Message, Message.CorrelationID) BusT ()
produce = forever $ do
    frame <- lift $ BusT Bus.receive
    liftIO $ logger debug $ "Received request: " ++ (frame ^. MessageFrame.message . Message.topic)
    Pipes.yield (frame ^. MessageFrame.message, frame ^. MessageFrame.correlation)


consume :: Pipes.Consumer (Message, Message.CorrelationID, Flag) BusT ()
consume = forever $ do
    (msg, crl, flag) <- Pipes.await
    liftIO $ logger debug $ "Sending reply: " ++ (msg ^. Message.topic)
    void $ lift $ BusT $ Bus.reply crl flag msg


run :: BusEndPoints -> HandlerMap s m
    -> IO (Pipes.Input  (Message, Message.CorrelationID),
           Pipes.Output (Message, Message.CorrelationID, Flag))
run endPoints handlerMap = do
    (output1, input1) <- Pipes.spawn Pipes.Single
    (output2, input2) <- Pipes.spawn Pipes.Single
    let forkPipesThread fun = forkIO_ $ eitherStringToM' $ Bus.runBus endPoints $ do
                            mapM_ Bus.subscribe $ HandlerMap.topics handlerMap
                            BusT.runBusT $ Pipes.runEffect fun
    forkPipesThread $ produce >-> Pipes.toOutput output1
    forkPipesThread $ Pipes.fromInput input2 >-> consume
    return (input1, output2)

