
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Prelude                          hiding (error)

import qualified Flowbox.Options.Applicative      as Opt
import           Flowbox.Options.Applicative      hiding (info)
import           Flowbox.System.Log.Logger          
import qualified Flowbox.Batch.Server.ZMQ.Server  as Server
import           Flowbox.Batch.Server.ZMQ.Handler   (Handler)
import qualified Flowbox.Batch.Server.ZMQ.Handler as Handler
import           Generated.ServerApi.Ping_Request
import           Generated.ServerApi.Ping_Response


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ"


defaultAddress :: String
defaultAddress = "tcp://*:30521"


data BatchHandler = BatchHandler

instance Handler BatchHandler where
    ping h i = return $ Ping_Response Nothing


main :: IO()
main = do 
    let batchHandler = BatchHandler
    Server.serve defaultAddress batchHandler