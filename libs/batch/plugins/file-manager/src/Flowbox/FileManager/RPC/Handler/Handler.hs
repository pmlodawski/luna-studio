---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.FileManager.RPC.Handler.Handler where

import           Flowbox.Bus.Data.Topic                      (status, update, (/+))
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import           Flowbox.FileManager.FileManager             (FileManager)
import qualified Flowbox.FileManager.RPC.Handler.Directory   as DirectoryHandler
import qualified Flowbox.FileManager.RPC.Handler.File        as FileHandler
import qualified Flowbox.FileManager.RPC.Handler.FileManager as FileManager
import           Flowbox.Prelude                             hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


handlerMap :: FileManager fm ctx => fm -> HandlerMap ctx IO
handlerMap fm callback = HandlerMap.fromList
    [ ("filesystem.directory.fetch.request" , respond status $ DirectoryHandler.fetch  fm)
    , ("filesystem.directory.upload.request", respond status $ DirectoryHandler.upload fm)
    , ("filesystem.directory.exists.request", respond status $ DirectoryHandler.exists fm)
    , ("filesystem.directory.create.request", respond update $ DirectoryHandler.create fm)
    , ("filesystem.directory.list.request"  , respond status $ DirectoryHandler.list   fm)
    , ("filesystem.directory.remove.request", respond update $ DirectoryHandler.remove fm)
    , ("filesystem.directory.copy.request"  , respond update $ DirectoryHandler.copy   fm)
    , ("filesystem.directory.move.request"  , respond update $ DirectoryHandler.move   fm)
    , ("filesystem.file.fetch.request"      , respond status $ FileHandler.fetch   fm)
    , ("filesystem.file.upload.request"     , respond status $ FileHandler.upload  fm)
    , ("filesystem.file.exists.request"     , respond update $ FileHandler.exists  fm)
    , ("filesystem.file.remove.request"     , respond update $ FileHandler.remove  fm)
    , ("filesystem.file.copy.request"       , respond update $ FileHandler.copy    fm)
    , ("filesystem.file.move.request"       , respond update $ FileHandler.move    fm)
    , ("filemanager.stat.request"           , respond status $ FileManager.stat    fm)
    , ("filemanager.resolve.request"        , respond status $ FileManager.resolve fm)
    , ("filemanager.ping.request"           , respond status $ FileManager.ping      )
    ]
    where
        respond type_ = callback (/+ type_) . Processor.singleResult
