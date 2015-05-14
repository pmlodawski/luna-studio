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
import qualified Data.Map            as Map
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
import           Luna.Interpreter.Session.Data.VarName       (VarName (VarName))
import qualified Luna.Interpreter.Session.Data.VarName       as VarName
import qualified Luna.Interpreter.Session.Env                as Env
import qualified Luna.Interpreter.Session.Error              as Error
import           Luna.Interpreter.Session.Memory.Manager     (MemoryManager)
import qualified Luna.Interpreter.Session.Memory.Manager     as Manager
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session



logger :: LoggerIO
logger = getLoggerIO $moduleName


dump :: CallPointPath -> [Hash] -> Session mm ()
dump callPointPath hash = do
    let varName = VarName callPointPath hash
    logger debug $ "Dumping " ++ VarName.toString varName
    Session.runStmt $ "print " ++ VarName.toString varName


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


setStatus :: CacheStatus -> CallPointPath -> Session mm ()
setStatus newStatus = modifyCacheInfo $ CacheInfo.status .~ newStatus


dependency :: [VarName] -> CallPointPath -> Session mm (Maybe VarName)
dependency predVarNames = onCacheInfo
    (return . Map.lookup predVarNames . view CacheInfo.dependencies)
    (return Nothing)


recentVarName :: CallPointPath -> Session mm VarName
recentVarName = onCacheInfo
    (return . view CacheInfo.recentVarName)
    (return def)


setRecentVarName :: VarName -> CallPointPath -> Session mm ()
setRecentVarName varName = modifyCacheInfo (CacheInfo.recentVarName .~ varName)


modifyCacheInfo :: (CacheInfo -> CacheInfo) -> CallPointPath ->  Session mm ()
modifyCacheInfo f callPointPath = onCacheInfo
    (Env.cachedInsert callPointPath . f)
    --FIXME (left $ Error.OtherError $(loc) $ "Cannot find callPointPath = " ++ show callPointPath)
    (return ())
    callPointPath


onCacheInfo :: (CacheInfo -> Session mm a) -> Session mm a -> CallPointPath -> Session mm a
onCacheInfo f alternative callPointPath =
    Maybe.maybe alternative f . MapForest.lookup callPointPath =<< Env.getCached


put :: CallDataPath -> [VarName] -> VarName -> Session mm ()
put callDataPath predVarNames varName = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    mcacheInfo <- Env.cachedLookup callPointPath
    oldStatus  <- status callPointPath
    let updatedStatus = if oldStatus == CacheStatus.NonCacheable
                            then oldStatus
                            else CacheStatus.Ready
        existingDeps   = Maybe.maybe def (view CacheInfo.dependencies) mcacheInfo
        existingValues = Maybe.maybe def (view CacheInfo.values      ) mcacheInfo
        dependencies = Map.insert predVarNames varName existingDeps
        cacheInfo    = CacheInfo (last callDataPath ^. CallData.parentDefID)
                                 (last callDataPath ^. CallData.parentBC)
                                 updatedStatus varName dependencies existingValues

    Env.cachedInsert callPointPath cacheInfo


deleteNode :: MemoryManager mm => Library.ID -> Node.ID -> Session mm ()
deleteNode libraryID nodeID = do
    logger info $ "Cleaning node: " ++ show (libraryID, nodeID)
    let callPoint     = CallPoint libraryID nodeID
        matchNode k _ = last k == callPoint
    matching <- MapForest.find matchNode <$> Env.getCached
    mapM_ delete' matching
    dependent <- Env.getDependentNodesOf callPoint
    Env.deleteDependentNodes callPoint
    mapM_ (deleteNode libraryID) $ IntSet.toList dependent


delete :: MemoryManager mm => CallPointPath -> Session mm ()
delete callPointPath = do
    logger info $ "Cleaning cached value: " ++ show callPointPath
    cacheInfo <- getCacheInfo callPointPath
    delete' (callPointPath, cacheInfo)


delete' :: MemoryManager mm => (CallPointPath, CacheInfo) -> Session mm ()
delete' (callPointPath, cacheInfo) = do
    Free.freeCacheInfo cacheInfo
    Env.cachedDelete callPointPath
    Manager.reportDeleteMany $ Map.elems $ cacheInfo ^. CacheInfo.dependencies


deleteVarName :: VarName -> Session mm ()
deleteVarName varName = do
    onCacheInfo del err callPointPath
    where
        callPointPath = varName ^. VarName.callPointPath
        err = logger warning $ "Cannot find callPointPath = " ++ show callPointPath
        del cacheInfo = do
            if cacheInfo ^. CacheInfo.recentVarName == varName
                then Env.cachedDelete callPointPath
                else Env.cachedInsert callPointPath
                   $ clearDependencies $ clearValues cacheInfo
            Free.freeVarName varName
        clearDependencies = CacheInfo.dependencies %~ Map.filter (/= varName)
        clearValues       = CacheInfo.values       %~ Map.filterWithKey (\k _ -> varName /= fst k)


deleteAll :: MemoryManager mm => Session mm ()
deleteAll = do
    logger info "Cleaning all cached values"
    mapM_ delete' =<< MapForest.toList <$> Env.getCached


getCacheInfo :: CallPointPath -> Session mm CacheInfo
getCacheInfo callPointPath = Env.cachedLookup callPointPath
    <??&> Error.CacheError $(loc) (concat ["Object ", show callPointPath, " is not in cache."])


performGC :: Session mm ()
performGC = do
    logger info "Running GC"
    Session.runStmt "performGC"
    safeLiftIO' (Error.IOError $(loc)) Mem.performGC

