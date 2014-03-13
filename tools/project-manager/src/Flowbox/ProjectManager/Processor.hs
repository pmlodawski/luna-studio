---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Processor where

import qualified Data.ByteString.Char8 as Char8

import           Flowbox.Bus.Message       (Message)
import qualified Flowbox.Bus.Message       as Message
import qualified Flowbox.Bus.Topic.Action  as Action
import           Flowbox.Bus.Topic.Topic   (Topic)
import qualified Flowbox.Bus.Topic.Topic   as Topic
import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger

import           Flowbox.ProjectManager.Action.Project          (Project (Project))
import qualified Flowbox.ProjectManager.Action.ProjectOperation as P



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Processor"


topic :: Topic
topic = Topic.mk "project"


process :: Message -> IO Message
process msg = case Topic.fromTopic $ Message.topic msg of
    Right (Project P.Open Action.Request) -> return $ Message.Message (Topic.mk "some.answer") (Char8.pack "some_data")
    Right unsupported                     -> do logger error $ "Unknown topic: " ++ show unsupported
                                                return $ Message.Message (Topic.mk "some.error")  (Char8.pack "some_data")
    Left err                              -> do logger error $ "Colud not parse topic: " ++ show err
                                                return $ Message.Message (Topic.mk "some.error")  (Char8.pack "some_data")
