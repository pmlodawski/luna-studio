
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Prelude                                 hiding (error)

import qualified System.ZMQ3.Monadic                     as ZMQ3
import qualified Text.ProtocolBuffers.Basic              as Proto

import qualified Flowbox.Options.Applicative             as Opt
import           Flowbox.Options.Applicative             hiding (info)
import           Flowbox.System.Log.Logger                 
import qualified Flowbox.Batch.Server.ZMQ.Server         as Server
import           Flowbox.Batch.Server.ZMQ.Handler          (Handler)
import qualified Flowbox.Batch.Server.ZMQ.Handler        as Handler
import qualified Generated.ServerApi.Server.Ping.Args    as PingArgs
import qualified Generated.ServerApi.Server.Ping.Result  as PingResult
import qualified Generated.ServerApi.Server.Ping2.Args   as Ping2Args
import qualified Generated.ServerApi.Server.Ping2.Result as Ping2Result



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ"


defaultAddress :: String
defaultAddress = "tcp://*:30521"


data BatchHandler = BatchHandler

instance Handler BatchHandler where
    ping h i  = do ZMQ3.liftIO $ loggerIO info $ "Called ping: " ++ show i
                   return $ PingResult.Result $ Just $ Proto.uFromString "ping2"
    ping2 h i = do ZMQ3.liftIO $ loggerIO info $ "Called ping2: " ++ show i
                   return $ Ping2Result.Result $ Just $ Proto.uFromString "ping2"

main :: IO()
main = do 
    let batchHandler = BatchHandler
    rootLogger setIntLevel 0
    Server.serve defaultAddress batchHandler