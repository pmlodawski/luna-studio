---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Memory.Manager.LRU where

import           Flowbox.Data.IndexedSet                     (IndexedSet)
import qualified Flowbox.Data.IndexedSet                     as IndexedSet
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger                   as Logger
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Memory.Manager
import qualified Luna.Interpreter.Session.Memory.Status      as Status



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


data LRU = LRU { _recentlyUsed :: IndexedSet CallPointPath }
         deriving (Show)

makeLenses ''LRU


instance Default LRU where
    def = LRU def


instance MemoryManager LRU where
    reportUse cpp = Env.updateMemoryManager (recentlyUsed %~ IndexedSet.insert cpp)

    clean status = do
        logger info "Cleaning memory..."
        lru <- view recentlyUsed <$> Env.getMemoryManager
        print $ IndexedSet.toList lru
        logger warning $ show status
        logger info "Cleaning memory...done"

    cleanIfNeeded = do
        status <- Status.status
        when (Status.isUpperLimitExceeded status) $
            clean status

