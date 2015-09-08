---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

module Main where

import           Flowbox.Bus.Bus                 (Bus)
import qualified Flowbox.Bus.Bus                 as Bus
import qualified Flowbox.Bus.Data.Flag           as Flag
import qualified Flowbox.Bus.Data.Message        as Message
import qualified Flowbox.Bus.Data.MessageFrame   as MessageFrame
import           Flowbox.Bus.EndPoint            (BusEndPoints (BusEndPoints))
import qualified Flowbox.WSConnector.WSConnector as WSConnector

import qualified Data.ByteString.Char8 as Char8
import           Flowbox.Prelude
import qualified System.ZMQ4.Monadic   as ZMQ



main :: IO ()
main = WSConnector.run
