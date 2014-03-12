---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.BusClient where

import qualified System.ZMQ4.Monadic as ZMQ

import           Control.Monad          (forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as Char8

import           Flowbox.Bus.Bus           (Bus)
import qualified Flowbox.Bus.Bus           as Bus
import qualified Flowbox.Bus.Env           as Env
import           Flowbox.Bus.Message       (Message)
import qualified Flowbox.Bus.Message       as Message
import           Flowbox.Bus.MessageFrame  (MessageFrame (MessageFrame))
import qualified Flowbox.Bus.Topic.Action  as Action
import           Flowbox.Bus.Topic.Topic   (Topic)
import qualified Flowbox.Bus.Topic.Topic   as Topic
import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger

import           Flowbox.ProjectManager.Action.Project          (Project (Project))
import qualified Flowbox.ProjectManager.Action.ProjectOperation as P



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.BusClient"


topic :: Topic
topic = Topic.mk "project"


run :: Env.BusEndPoints -> IO (Either String ())
run endPoints = ZMQ.runZMQ $ Bus.runBus run' endPoints


run' :: Bus ()
run' = do
    Bus.subscribe topic
    _ <- forever $ handle
    return ()


handle :: Bus ()
handle = do
    request <- Bus.receive
    case request of
        Left err -> liftIO $ logger error $ "Error while decoding message: " ++ err
        Right (MessageFrame msg crlID _) -> do
            liftIO $ logger debug $ "Received request: " ++ (Topic.str $ Message.topic msg)
            response <- liftIO $ process msg
            Bus.send (response, crlID)


process :: Message -> IO Message
process msg = case Topic.fromTopic $ Message.topic msg of
    Right (Project P.Open Action.Request) -> return $ Message.Message (Topic.mk "some.answer") (Char8.pack "some_data")
    Right unsupported                     -> do logger error $ "Unknown topic: " ++ show unsupported
                                                return $ Message.Message (Topic.mk "some.error")  (Char8.pack "some_data")
    Left err                              -> do logger error $ "Colud not parse topic: " ++ show err
                                                return $ Message.Message (Topic.mk "some.error")  (Char8.pack "some_data")
