
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Control.BrokerCtx where

import           Control.Applicative
import           Data.IORef          (IORef)
import qualified Data.IORef          as IORef

import Flowbox.Prelude



newtype BrokerCtx = BrokerCtx { nextSenderID :: IORef Int }


empty :: IO BrokerCtx
empty = BrokerCtx <$> IORef.newIORef 0
