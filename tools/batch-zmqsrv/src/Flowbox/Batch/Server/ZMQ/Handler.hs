
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handler where


import           Control.Monad                 (forever)
import qualified Data.ByteString.Char8       as Char8
import qualified System.ZMQ3.Monadic         as ZMQ3

import           Flowbox.Prelude             hiding (error)
import qualified Flowbox.Options.Applicative as Opt
import           Flowbox.Options.Applicative hiding (info)
import           Flowbox.System.Log.Logger     
import           Generated.ServerApi.Ping_Request
import           Generated.ServerApi.Ping_Response



class Handler h where
    ping :: h -> Ping_Request -> ZMQ3.ZMQ z Ping_Response