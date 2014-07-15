---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Bus.RPC.Pipes where

import Pipes

import           Flowbox.Bus.Bus               (BusT)
import qualified Flowbox.Bus.Bus               as Bus
import qualified Flowbox.Bus.Data.Flag         as Flag
import           Flowbox.Bus.Data.Message      (Message)
import qualified Flowbox.Bus.Data.Message      as Message
import qualified Flowbox.Bus.Data.MessageFrame as MessageFrame
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Prelude               hiding (error)



produce :: Producer (Message, Message.CorrelationID) BusT ()
produce = do
    frame <- lift $ Bus.BusT $ Bus.receive
    yield (frame ^. MessageFrame.message, frame ^. MessageFrame.correlation)


consume :: Consumer (Message, Message.CorrelationID) BusT ()
consume = do
    (msg, crl) <- await
    void $ lift $ Bus.BusT $ Bus.reply crl Flag.Enable msg


run :: BusEndPoints
    -> Pipe (Message, Message.CorrelationID) (Message, Message.CorrelationID) BusT ()
    -> IO (Either Bus.Error ())
run busEndPoints action = Bus.runBus busEndPoints $ Bus.runBusT $ runEffect
    $ produce >-> action >-> consume
--f :: Producer MessageFrame.MessageFrame a ()
--f = lift $ Bus.receive


--a :: Bus.Bus ()
--a = Bus.runBusT $ runEffect $ produce >-> consume
