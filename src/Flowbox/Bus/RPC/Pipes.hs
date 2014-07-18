---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Flowbox.Bus.RPC.Pipes where

import Pipes

import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.BusT              (BusT (BusT))
import qualified Flowbox.Bus.BusT              as BusT
import qualified Flowbox.Bus.Data.Flag         as Flag
import           Flowbox.Bus.Data.Message      (Message)
import qualified Flowbox.Bus.Data.Message      as Message
import qualified Flowbox.Bus.Data.MessageFrame as MessageFrame
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Bus.RPC.HandlerMap    (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap    as HandlerMap
import           Flowbox.Prelude               hiding (error)



produce :: Producer (Message, Message.CorrelationID) BusT ()
produce = do
    frame <- lift $ BusT $ Bus.receive
    yield (frame ^. MessageFrame.message, frame ^. MessageFrame.correlation)


consume :: Consumer (Message, Message.CorrelationID) BusT ()
consume = do
    (msg, crl) <- await
    void $ lift $ BusT $ Bus.reply crl Flag.Enable msg


run :: MonadIO m => BusEndPoints -> HandlerMap n
    -> Pipe (Message, Message.CorrelationID) (Message, Message.CorrelationID) BusT ()
    -> m (Either Bus.Error ())
run endPoints handlerMap handler = Bus.runBus endPoints $ do
    mapM_ Bus.subscribe $ HandlerMap.topics handlerMap
    BusT.runBusT $ runEffect $ produce >-> handler >-> consume
