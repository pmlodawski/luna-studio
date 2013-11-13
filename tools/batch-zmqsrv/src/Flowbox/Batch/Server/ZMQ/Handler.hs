
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handler where

import qualified System.ZMQ3.Monadic                     as ZMQ3

import qualified Generated.ServerApi.Server.Ping.Call    as PingCall
import qualified Generated.ServerApi.Server.Ping.Result  as PingResult
import qualified Generated.ServerApi.Server.Ping2.Call   as Ping2Call
import qualified Generated.ServerApi.Server.Ping2.Result as Ping2Result


class Handler h where
    ping :: h -> PingCall.Call -> ZMQ3.ZMQ z PingResult.Result
    ping2 :: h -> Ping2Call.Call -> ZMQ3.ZMQ z Ping2Result.Result