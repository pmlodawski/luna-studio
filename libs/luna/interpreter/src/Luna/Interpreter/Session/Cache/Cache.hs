---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Cache.Cache where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.IntSet         as IntSet
import qualified Data.Maybe          as Maybe
import qualified System.Mem          as Mem

import           Flowbox.Control.Error                       hiding (err)
import qualified Flowbox.Data.MapForest                      as MapForest
import           Flowbox.Prelude                             hiding (matching)
import           Flowbox.Source.Location                     (loc)
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.Graph.Node                         as Node
import qualified Luna.DEP.Lib.Lib                            as Library
import qualified Luna.Interpreter.Session.Cache.Free         as Free
import           Luna.Interpreter.Session.Cache.Info         (CacheInfo (CacheInfo))
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import           Luna.Interpreter.Session.Cache.Status       (CacheStatus)
import qualified Luna.Interpreter.Session.Cache.Status       as CacheStatus
import qualified Luna.Interpreter.Session.Data.CallData      as CallData
import           Luna.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Luna.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.Hash          (Hash)
import           Luna.Interpreter.Session.Data.KeyName       (KeyName (KeyName))
import qualified Luna.Interpreter.Session.Env                as Env
import qualified Luna.Interpreter.Session.Error              as Error
import           Luna.Interpreter.Session.Memory.Manager     (MemoryManager)
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session



logger :: LoggerIO
logger = getLoggerIO $moduleName


dumpAll :: Session mm ()
dumpAll = do
    --logger trace =<< MapForest.draw <$> Env.getCached
    logger trace =<< MapForest.drawKeys <$> Env.getCached
    logger trace "====================="
    logger trace =<< show <$> Env.getDependentNodes


isDirty :: CallPointPath -> Session mm Bool
isDirty = onCacheInfo
    (\cacheInfo -> return $ cacheInfo ^. CacheInfo.status /= CacheStatus.Ready)
    (return True)


status :: CallPointPath -> Session mm CacheStatus
status = onCacheInfo
    (return . view CacheInfo.status)
    (return CacheStatus.Modified)


mergeStatus :: CacheStatus -> CallPointPath -> Session mm ()
mergeStatus newStatus = modifyCacheInfo $ CacheInfo.status %~ CacheStatus.merge newStatus


recentHash :: CallPointPath -> Session mm Hash
recentHash = onCacheInfo
    (return . view CacheInfo.recentHash)
    (return def)


setRecentHash :: Hash -> CallPointPath -> Session mm ()
setRecentHash hash = modifyCacheInfo (CacheInfo.recentHash .~ hash)


modifyCacheInfo :: (CacheInfo -> CacheInfo) -> CallPointPath ->  Session mm ()
modifyCacheInfo f callPointPath = onCacheInfo
    (Env.cachedInsert callPointPath . f)
    --FIXME (left $ Error.OtherError $(loc) $ "Cannot find callPointPath = " ++ show callPointPath)
    (return ())
    callPointPath


onCacheInfo :: (CacheInfo -> Session mm a) -> Session mm a -> CallPointPath -> Session mm a
onCacheInfo f alternative callPointPath =
    Maybe.maybe alternative f . MapForest.lookup callPointPath =<< Env.getCached


put :: CallDataPath -> Hash -> Session mm ()
put callDataPath hash = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    mcacheInfo <- Env.cachedLookup callPointPath
    oldStatus  <- status callPointPath
    let updatedStatus = if oldStatus == CacheStatus.NonCacheable
                            then oldStatus
                            else CacheStatus.Ready
        existingValues = Maybe.maybe def (view CacheInfo.values) mcacheInfo
        cacheInfo    = CacheInfo (last callDataPath ^. CallData.parentDefID)
                                 (last callDataPath ^. CallData.parentBC)
                                 updatedStatus hash existingValues

    Env.cachedInsert callPointPath cacheInfo


deleteNode :: MemoryManager mm => Library.ID -> Node.ID -> Session mm ()
deleteNode libraryID nodeID = do
    logger info $ "Cleaning node: " ++ show (libraryID, nodeID)
    let callPoint     = CallPoint libraryID nodeID
        matchNode k _ = last k == callPoint
    matching <- MapForest.find matchNode <$> Env.getCached
    mapM_ (delete . fst) matching
    dependent <- Env.getDependentNodesOf callPoint
    Env.deleteDependentNodes callPoint
    mapM_ (deleteNode libraryID) $ IntSet.toList dependent


delete :: MemoryManager mm => CallPointPath -> Session mm ()
delete callPointPath = do
    logger info $ "Cleaning cached value: " ++ show callPointPath
    Free.freeKeyName (KeyName callPointPath)
    Env.cachedDelete callPointPath


deleteAll :: MemoryManager mm => Session mm ()
deleteAll = do
    logger info "Cleaning all cached values"
    mapM_ (delete . fst) =<< MapForest.toList <$> Env.getCached


getCacheInfo :: CallPointPath -> Session mm CacheInfo
getCacheInfo callPointPath = Env.cachedLookup callPointPath
    <??&> Error.CacheError $(loc) (concat ["Object ", show callPointPath, " is not in cache."])


performGC :: Session mm ()
performGC = do
    logger info "Running GC"
    Session.runStmt "performGC"
    safeLiftIO' (Error.IOError $(loc)) Mem.performGC
