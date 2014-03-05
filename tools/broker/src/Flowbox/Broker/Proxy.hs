---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Proxy where

import           System.ZMQ4.Monadic (ZMQ)
import qualified System.ZMQ4.Monadic as ZMQ

import Flowbox.Prelude



run :: String -> String -> IO ()
run pullAddr pubAddr = ZMQ.runZMQ $ serve pullAddr pubAddr


serve :: String -> String -> ZMQ z ()
serve pullAddr pubAddr = do
    pull      <- ZMQ.socket ZMQ.Pull
    ZMQ.bind pull pullAddr

    pub <- ZMQ.socket ZMQ.Pub
    --setSendHighWM (restrict nbOfUpdate) publisher
    ZMQ.bind pub pubAddr

    ZMQ.proxy pull pub Nothing
