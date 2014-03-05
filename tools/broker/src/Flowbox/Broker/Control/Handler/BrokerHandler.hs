
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Control.Handler.BrokerHandler where

import           Control.Applicative
import           Data.IORef          (IORef)
import qualified Data.IORef          as IORef

import           Flowbox.Broker.Control.Handler.Handler (Handler (..))
import qualified Flowbox.Broker.Control.Handler.ID      as HID
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Broker.Control.Handler.BrokerHandler"


data BrokerHandler = BrokerHandler { senderID :: IORef Int
                                   }


empty :: IO BrokerHandler
empty = BrokerHandler <$> IORef.newIORef 0


instance Handler BrokerHandler where
    newID h = HID.new (senderID h)
