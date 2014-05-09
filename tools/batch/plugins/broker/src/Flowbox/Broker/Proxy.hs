---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Proxy where

import           System.ZMQ4.Monadic (ZMQ)
import qualified System.ZMQ4.Monadic as ZMQ

import Flowbox.Bus.EndPoint (EndPoint)
import Flowbox.Prelude


run :: EndPoint -> EndPoint -> IO ()
run pullAddr pubAddr = ZMQ.runZMQ $ serve pullAddr pubAddr


serve :: EndPoint -> EndPoint -> ZMQ z ()
serve pullAddr pubAddr = do
    pull <- ZMQ.socket ZMQ.Pull
    pub  <- ZMQ.socket ZMQ.Pub
    ZMQ.bind pull pullAddr
    ZMQ.bind pub pubAddr
    ZMQ.proxy pull pub Nothing
