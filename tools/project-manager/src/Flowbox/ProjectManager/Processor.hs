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
import           Flowbox.Bus.Topic         (Topic)
import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Processor"


topics :: [Topic]
topics = ["project"]


process :: ctx -> Message -> IO Message
process ctx msg = case Message.topic msg of
    "project.open.request" -> return $ Message.Message "some.answer" (Char8.pack "some_data")
    unsupported            -> do logger error $ "Unknown topic: " ++ show unsupported
                                 return $ Message.Message "some.error" (Char8.pack "some_data")
