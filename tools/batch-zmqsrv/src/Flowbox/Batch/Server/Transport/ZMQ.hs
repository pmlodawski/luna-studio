
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Transport.ZMQ where

import           Control.Applicative
import           Control.Monad        (forever)
import qualified Data.ByteString.Lazy as ByteString
import qualified System.ZMQ3.Monadic  as ZMQ3

import           Flowbox.Batch.Server.Handler.Handler (Handler)
import qualified Flowbox.Batch.Server.Processor       as Processor
import           Flowbox.Prelude                      hiding (error)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ"


serve :: Handler h => String -> Int -> h -> IO ()
serve address port handler = ZMQ3.runZMQ $ do
    socket <- ZMQ3.socket ZMQ3.Rep
    ZMQ3.bind socket (address ++ ":" ++ show port)
    forever $ handleCall socket handler


handleCall :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
handleCall socket handler = do
    encoded_request  <- ByteString.fromStrict <$> ZMQ3.receive socket
    encoded_response <- ZMQ3.liftIO $ ByteString.toStrict <$> Processor.process handler encoded_request
    ZMQ3.send socket [] $ encoded_response
