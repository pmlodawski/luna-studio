
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Server where


import           Control.Monad                               (forever)
import qualified System.ZMQ3.Monadic                       as ZMQ3

import           Flowbox.Prelude                           hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Processor        as Processor
import           Flowbox.Batch.Server.ZMQ.Handlers.Handler   (Handler)
import           Flowbox.System.Log.Logger                   



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Server"


serve :: Handler h => String -> Int -> h -> IO ()
serve address port handler = ZMQ3.runZMQ $ do  
    socket <- ZMQ3.socket ZMQ3.Rep
    ZMQ3.bind socket (address ++ ":" ++ show port)
    forever $ Processor.process socket handler
