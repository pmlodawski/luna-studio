
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Server where


import           Control.Monad                        (forever)
import qualified Data.ByteString.Char8              as Char8
import qualified System.ZMQ3.Monadic                as ZMQ3

import           Flowbox.Prelude                    hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Processor as Processor
import qualified Flowbox.Batch.Server.ZMQ.Handler   as Handler
import           Flowbox.Batch.Server.ZMQ.Handler     (Handler)
import qualified Flowbox.Options.Applicative        as Opt
import           Flowbox.Options.Applicative        hiding (info)
import           Flowbox.System.Log.Logger            



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Server"


serve :: Handler h => String -> h -> IO ()
serve address handler = ZMQ3.runZMQ $ do  
    ZMQ3.liftIO $ putStrLn "Starting Hello World server"
    repSocket <- ZMQ3.socket ZMQ3.Rep
    ZMQ3.bind repSocket address

    forever $ Processor.process repSocket handler
        --msg <- ZMQ3.receive repSocket
        --msg <- ZMQ3.receive repSocket
        
        --(ZMQ3.liftIO.putStrLn.show.length.Char8.unpack) msg

        ---- Simulate doing some 'work' for 1 second
        ----liftIO $ threadDelay (1 * 1000 * 1000)

        --ZMQ3.send repSocket [] (Char8.pack "World")
