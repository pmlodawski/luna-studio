---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Control.Control where

import           System.ZMQ4.Monadic (ZMQ)
import qualified System.ZMQ4.Monadic as ZMQ

import Flowbox.Prelude


run :: String -> IO ()
run ctrlAddr = ZMQ.runZMQ $ serve ctrlAddr
--run  = ZMQ.runZMQ . serve 


serve :: String -> ZMQ z ()
serve ctrlAddr = do
    rep <- ZMQ.socket ZMQ.Rep
    ZMQ.bind rep ctrlAddr

    -- ......
    return ()
