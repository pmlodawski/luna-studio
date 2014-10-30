---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Cache.Cache where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe
import qualified Data.Set            as Set

import           Flowbox.Control.Error
import qualified Flowbox.Data.MapForest                      as MapForest
import           Flowbox.Prelude                             hiding (matching)
import           Flowbox.Source.Location                     (loc)
import           Flowbox.System.Log.Logger
import qualified Luna.Graph.Node                             as Node
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
import           Luna.Interpreter.Session.Data.VarName       (VarName)
import qualified Luna.Interpreter.Session.Data.VarName       as VarName
import qualified Luna.Interpreter.Session.Env                as Env
import qualified Luna.Interpreter.Session.Error              as Error
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session
import qualified Luna.Lib.Lib                                as Library



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


dump :: CallPointPath -> Maybe Hash -> Session ()
dump callPointPath mhash = do
    let varName = VarName.mk mhash callPointPath
    logger debug $ "Dumping " ++ varName
    Session.runStmt $ "print " ++ varName


dumpAll :: Session ()
dumpAll = logger trace =<< MapForest.draw <$> Env.getCached


isDirty :: CallPointPath -> Session Bool
isDirty = onCacheInfo
    (\cacheInfo -> return $ cacheInfo ^. CacheInfo.status /= CacheStatus.Ready)
    (return True)


status :: CallPointPath -> Session CacheStatus
status = onCacheInfo
    (return . view CacheInfo.status)
    (return CacheStatus.Modified)


setStatus :: CacheStatus -> CallPointPath -> Session ()
setStatus newStatus = modifyCacheInfo (CacheInfo.status .~ newStatus)


dependency :: [VarName] -> CallPointPath -> Session (Maybe VarName)
dependency predVarNames = onCacheInfo
    (return . Map.lookup predVarNames . view CacheInfo.dependencies)
    (return Nothing)


recentVarName :: CallPointPath -> Session VarName
recentVarName = onCacheInfo
    (return . view CacheInfo.recentVarName)
    (return "")


setRecentVarName :: VarName -> CallPointPath -> Session ()
setRecentVarName varName = modifyCacheInfo (CacheInfo.recentVarName .~ varName)


modifyCacheInfo :: (CacheInfo -> CacheInfo) -> CallPointPath ->  Session ()
modifyCacheInfo f callPointPath = onCacheInfo
    (Env.cachedInsert callPointPath . f)
    (return ())
    callPointPath


onCacheInfo :: (CacheInfo -> Session a) -> Session a -> CallPointPath -> Session a
onCacheInfo f alternative callPointPath =
    Maybe.maybe alternative f . MapForest.lookup callPointPath =<< Env.getCached


put :: CallDataPath -> [VarName] -> VarName -> Session ()
put callDataPath predVarNames varName = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    mcacheInfo <- Env.cachedLookup callPointPath
    oldStatus  <- status callPointPath
    let updatedStatus = if oldStatus == CacheStatus.NonCacheable
                            then oldStatus
                            else CacheStatus.Ready
        existingDeps = Maybe.maybe Map.empty (view CacheInfo.dependencies) mcacheInfo
        dependencies = Map.insert predVarNames varName existingDeps
        cacheInfo    = CacheInfo (last callDataPath ^. CallData.parentDefID)
                                 (last callDataPath ^. CallData.parentBC)
                                 updatedStatus varName dependencies

    Env.cachedInsert callPointPath cacheInfo


deleteNode :: Library.ID -> Node.ID -> Session ()
deleteNode libraryID nodeID = do
    logger info $ "Cleaning node: " ++ show (libraryID, nodeID)
    let callPoint     = CallPoint libraryID nodeID
        matchNode k _ = last k == callPoint
    matching <- MapForest.find matchNode <$> Env.getCached
    mapM_ delete' matching
    dependent <- Env.getDependentNodesOf callPoint
    Env.deleteDependentNodes callPoint
    mapM_ (deleteNode libraryID) $ Set.toList dependent


delete :: CallPointPath -> Session ()
delete callPointPath = do
    logger info $ "Cleaning cached value: " ++ show callPointPath
    cacheInfo <- getCacheInfo callPointPath
    delete' (callPointPath, cacheInfo)


delete' :: (CallPointPath, CacheInfo) -> Session ()
delete' (callPointPath, cacheInfo) = do
    Free.freeCacheInfo cacheInfo
    Env.cachedDelete callPointPath


deleteAll :: Session ()
deleteAll = do
    logger info "Cleaning all cached values"
    mapM_ delete' =<< MapForest.toList <$> Env.getCached


getCacheInfo :: CallPointPath -> Session CacheInfo
getCacheInfo callPointPath = Env.cachedLookup callPointPath
    <??&> Error.CacheError $(loc) (concat ["Object ", show callPointPath, " is not in cache."])
