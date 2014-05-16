---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.FileManager.Handler.Handler where

import           Flowbox.Bus.Data.Topic                (Topic)
import           Flowbox.Bus.RPC.BusRPCHandler         (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor             as P
import qualified Flowbox.FileManager.Handler.Directory as DirectoryHandler
import qualified Flowbox.FileManager.Handler.File      as FileHandler
import           Flowbox.Prelude                       hiding (Context, error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.FileManager.Handler"


topics :: [Topic]
topics = [ "filesystem.directory.fetch.request"
         , "filesystem.directory.upload.request"
         , "filesystem.directory.exists.request"
         , "filesystem.directory.create.request"
         , "filesystem.directory.list.request"
         , "filesystem.directory.remove.request"
         , "filesystem.directory.copy.request"
         , "filesystem.directory.move.request"
         , "filesystem.file.fetch.request"
         , "filesystem.file.upload.request"
         , "filesystem.file.exists.request"
         , "filesystem.file.remove.request"
         , "filesystem.file.copy.request"
         , "filesystem.file.move.request"
         ]


handler :: BusRPCHandler
handler callback topic = case topic of
    "filesystem.directory.fetch.request"  -> callback P.status $ P.singleResult DirectoryHandler.fetch
    "filesystem.directory.upload.request" -> callback P.status $ P.singleResult DirectoryHandler.upload
    "filesystem.directory.exists.request" -> callback P.update $ P.singleResult DirectoryHandler.exists
    "filesystem.directory.create.request" -> callback P.update $ P.singleResult DirectoryHandler.create
    "filesystem.directory.list.request"   -> callback P.status $ P.singleResult DirectoryHandler.list
    "filesystem.directory.remove.request" -> callback P.update $ P.singleResult DirectoryHandler.remove
    "filesystem.directory.copy.request"   -> callback P.update $ P.singleResult DirectoryHandler.copy
    "filesystem.directory.move.request"   -> callback P.update $ P.singleResult DirectoryHandler.move
    "filesystem.file.fetch.request"  -> callback P.status $ P.singleResult FileHandler.fetch
    "filesystem.file.upload.request" -> callback P.status $ P.singleResult FileHandler.upload
    "filesystem.file.exists.request" -> callback P.update $ P.singleResult FileHandler.exists
    "filesystem.file.remove.request" -> callback P.update $ P.singleResult FileHandler.remove
    "filesystem.file.copy.request"   -> callback P.update $ P.singleResult FileHandler.copy
    "filesystem.file.move.request"   -> callback P.update $ P.singleResult FileHandler.move
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg
