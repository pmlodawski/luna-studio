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
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.VarName       (VarName)
import qualified Luna.Interpreter.Session.Env                as Env
import qualified Luna.Interpreter.Session.Env.State          as Session
import           Luna.Interpreter.Session.Memory.Manager
import qualified Luna.Interpreter.Session.Memory.Status      as Status



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type Entry = (CallPointPath, VarName)


data LRU = LRU { _recentlyUsed :: IndexedSet Entry }
         deriving (Show)

makeLenses ''LRU


instance Default LRU where
    def = LRU def


instance MemoryManager LRU where
    reportUse cpp varName = Env.updateMemoryManager (recentlyUsed %~ IndexedSet.insert (cpp, varName))
    reportDelete cpp varName = Env.updateMemoryManager (recentlyUsed %~ IndexedSet.delete (cpp, varName))
    clean status = do
        logger info "Cleaning memory..."
        lru <- view recentlyUsed <$> Env.getMemoryManager
        let lruList = IndexedSet.toList lru
        performCleaning lruList
        logger warning $ show status
        logger info "Cleaning memory...quitting"
    cleanIfNeeded = do
        status <- Status.status
        when (Status.isUpperLimitExceeded status) $
            clean status


performCleaning :: [Entry] -> Session LRU ()
performCleaning [] = do logger warning "Cleaning requested but no items to clean!"
                        Env.setMemoryManager $ LRU IndexedSet.empty
performCleaning entries@(h:t) = do
    limitExceeded <- Status.isLowerLimitExceeded'
    if limitExceeded
        then uncurry Cache.deleteVarName h >> performCleaning t
        else Env.setMemoryManager $ LRU $ IndexedSet.fromList entries


