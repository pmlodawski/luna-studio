---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.FileManager.Handler.Handler where

import           Flowbox.Bus.Data.Message              (Message)
import qualified Flowbox.Bus.Data.Topic                as Topic
import           Flowbox.Bus.RPC.HandlerMap            (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap            as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor      as Processor
import qualified Flowbox.FileManager.Handler.Directory as DirectoryHandler
import qualified Flowbox.FileManager.Handler.File      as FileHandler
import           Flowbox.Prelude                       hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers          as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.FileManager.Handler"


handlerMap ::  HandlerMap
handlerMap callback = HandlerMap.fromList $
    [ ("filesystem.directory.fetch.request" , call Topic.status $ DirectoryHandler.fetch)
    , ("filesystem.directory.upload.request", call Topic.status $ DirectoryHandler.upload)
    , ("filesystem.directory.exists.request", call Topic.update $ DirectoryHandler.exists)
    , ("filesystem.directory.create.request", call Topic.update $ DirectoryHandler.create)
    , ("filesystem.directory.list.request"  , call Topic.status $ DirectoryHandler.list)
    , ("filesystem.directory.remove.request", call Topic.update $ DirectoryHandler.remove)
    , ("filesystem.directory.copy.request"  , call Topic.update $ DirectoryHandler.copy)
    , ("filesystem.directory.move.request"  , call Topic.update $ DirectoryHandler.move)
    , ("filesystem.file.fetch.request" , call Topic.status $ FileHandler.fetch)
    , ("filesystem.file.upload.request", call Topic.status $ FileHandler.upload)
    , ("filesystem.file.exists.request", call Topic.update $ FileHandler.exists)
    , ("filesystem.file.remove.request", call Topic.update $ FileHandler.remove)
    , ("filesystem.file.copy.request"  , call Topic.update $ FileHandler.copy)
    , ("filesystem.file.move.request"  , call Topic.update $ FileHandler.move)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> IO result) -> IO [Message]
        call type_ = callback type_ . Processor.singleResult
