---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.FileManager.RPC.Handler.Handler where

import Control.Monad.Trans.State

import           Flowbox.Bus.Data.Message                    (Message)
import           Flowbox.Bus.Data.Topic                      (Topic, status, update, (/+))
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import           Flowbox.Bus.RPC.RPC                         (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import           Flowbox.FileManager.Context                 (Context)
import qualified Flowbox.FileManager.RPC.Handler.Directory   as DirectoryHandler
import qualified Flowbox.FileManager.RPC.Handler.File        as FileHandler
import qualified Flowbox.FileManager.RPC.Handler.FileManager as FileManager
import           Flowbox.Prelude                             hiding (Context, error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                as Proto



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


handlerMap :: HandlerMap Context IO
handlerMap callback = HandlerMap.fromList
    [ ("filesystem.directory.fetch.request" , respond status $ DirectoryHandler.fetch )
    , ("filesystem.directory.upload.request", respond status $ DirectoryHandler.upload)
    , ("filesystem.directory.exists.request", respond status $ DirectoryHandler.exists)
    , ("filesystem.directory.create.request", respond update $ DirectoryHandler.create)
    , ("filesystem.directory.list.request"  , respond status $ DirectoryHandler.list  )
    , ("filesystem.directory.remove.request", respond update $ DirectoryHandler.remove)
    , ("filesystem.directory.copy.request"  , respond update $ DirectoryHandler.copy  )
    , ("filesystem.directory.move.request"  , respond update $ DirectoryHandler.move  )
    , ("filesystem.file.fetch.request" , respond status $ FileHandler.fetch )
    , ("filesystem.file.upload.request", respond status $ FileHandler.upload)
    , ("filesystem.file.exists.request", respond update $ FileHandler.exists)
    , ("filesystem.file.remove.request", respond update $ FileHandler.remove)
    , ("filesystem.file.copy.request"  , respond update $ FileHandler.copy  )
    , ("filesystem.file.move.request"  , respond update $ FileHandler.move  )
    , ("filemanager.ping.request"      , respond status FileManager.ping)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC Context IO result) -> StateT Context IO [Message]
        respond type_ = callback (/+ type_) . Processor.singleResult
