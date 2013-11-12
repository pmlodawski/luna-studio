
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.ZMQ.Processor where


import           Control.Monad                      (forever)
import qualified Data.ByteString.Char8            as Char8
import qualified Data.ByteString.Char8            as Char8
import qualified Data.ByteString.Lazy             as BSLazy
import qualified System.ZMQ3.Monadic              as ZMQ3
import qualified Text.ProtocolBuffers        as PB

import qualified Text.ProtocolBuffers.Basic
import qualified Text.ProtocolBuffers.Extensions
import qualified Text.ProtocolBuffers.Identifiers
import qualified Text.ProtocolBuffers.Reflections
import qualified Text.ProtocolBuffers.WireMessage

import           Flowbox.Prelude                  hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Handler as Handler
import           Flowbox.Batch.Server.ZMQ.Handler   (Handler)
import qualified Flowbox.Options.Applicative      as Opt
import           Flowbox.Options.Applicative      hiding (info)
import           Flowbox.System.Log.Logger          



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Processor"


process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    encoded_method <- ZMQ3.receive socket
    encoded_args   <- ZMQ3.receive socket

    let method = Char8.unpack encoded_method

    case PB.messageGet $ BSLazy.fromStrict encoded_args of
        Left err -> fail $ "Error while decoding args: " ++ err
        Right (args, _) -> do
            result <- case method of 
                "ping" -> Handler.ping handler args
    
            let encoded_result = BSLazy.toStrict $ PB.messagePut result
            ZMQ3.send socket [] encoded_result
    
    --args <- ZMQ3.receive
    --result <- case command of
    --    "ping" -> ping handler args


        
    --    (ZMQ3.liftIO.putStrLn.show.length.Char8.unpack) msg

    --    -- Simulate doing some 'work' for 1 second
    --    --liftIO $ threadDelay (1 * 1000 * 1000)

