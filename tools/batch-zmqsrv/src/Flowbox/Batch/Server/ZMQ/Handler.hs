
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handler where

import qualified System.ZMQ3.Monadic                             as ZMQ3

import           Generated.ServerApi.Server.Ping.Ping_Request      
import           Generated.ServerApi.Server.Ping.Ping_Response     
import           Generated.ServerApi.Server.Ping2.Ping2_Request    
import           Generated.ServerApi.Server.Ping2.Ping2_Response   



class Handler h where
    ping :: h -> Ping_Request -> ZMQ3.ZMQ z Ping_Response
    ping2 :: h -> Ping2_Request -> ZMQ3.ZMQ z Ping2_Response