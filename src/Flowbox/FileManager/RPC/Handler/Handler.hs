---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.FileManager.RPC.Handler.Handler where

import           Flowbox.Bus.Data.Message                  (Message)
import           Flowbox.Bus.Data.Topic                    (Topic, status, update, (/+))
import           Flowbox.Bus.RPC.HandlerMap                (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                as HandlerMap
import           Flowbox.Bus.RPC.RPC                       (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor          as Processor
import           Flowbox.FileManager.Context               (Context)
import qualified Flowbox.FileManager.RPC.Handler.Directory as DirectoryHandler
import qualified Flowbox.FileManager.RPC.Handler.File      as FileHandler
import           Flowbox.Prelude                           hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers              as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.FileManager.Handler"


handlerMap :: Context -> HandlerMap IO
handlerMap ctx callback = HandlerMap.fromList
    [ ("filesystem.directory.fetch.request" , respond status $ DirectoryHandler.fetch  ctx)
    , ("filesystem.directory.upload.request", respond status $ DirectoryHandler.upload ctx)
    , ("filesystem.directory.exists.request", respond update $ DirectoryHandler.exists ctx)
    , ("filesystem.directory.create.request", respond update $ DirectoryHandler.create ctx)
    , ("filesystem.directory.list.request"  , respond status $ DirectoryHandler.list   ctx)
    , ("filesystem.directory.remove.request", respond update $ DirectoryHandler.remove ctx)
    , ("filesystem.directory.copy.request"  , respond update $ DirectoryHandler.copy   ctx)
    , ("filesystem.directory.move.request"  , respond update $ DirectoryHandler.move   ctx)
    , ("filesystem.file.fetch.request" , respond status $ FileHandler.fetch  ctx)
    , ("filesystem.file.upload.request", respond status $ FileHandler.upload ctx)
    , ("filesystem.file.exists.request", respond update $ FileHandler.exists ctx)
    , ("filesystem.file.remove.request", respond update $ FileHandler.remove ctx)
    , ("filesystem.file.copy.request"  , respond update $ FileHandler.copy   ctx)
    , ("filesystem.file.move.request"  , respond update $ FileHandler.move   ctx)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC IO result) -> IO [Message]
        respond type_ = callback ((/+) type_) . Processor.singleResult
