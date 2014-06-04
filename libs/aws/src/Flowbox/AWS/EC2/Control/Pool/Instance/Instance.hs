---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.EC2.Control.Pool.Instance.Instance where

import qualified AWS.EC2.Types           as Types
import qualified Control.Concurrent      as Concurrent
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad           (forever)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map                as Map

import           Flowbox.AWS.EC2.Control.Pool.Instance.Info  (InstanceInfo (InstanceInfo))
import qualified Flowbox.AWS.EC2.Control.Pool.Instance.Info  as InstanceInfo
import qualified Flowbox.AWS.EC2.Control.Pool.Instance.State as InstanceState
import           Flowbox.AWS.EC2.Control.Pool.Pool           (MPool, Pool)
import qualified Flowbox.AWS.EC2.Control.Pool.Pool           as Pool
import qualified Flowbox.AWS.EC2.Control.Pool.Tag            as Tag
import           Flowbox.AWS.EC2.EC2                         (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2                         as EC2
import qualified Flowbox.AWS.EC2.Instance.Instance           as Instance
import qualified Flowbox.AWS.EC2.Instance.Management         as Management
import qualified Flowbox.AWS.EC2.Instance.Tag                as Tag
import qualified Flowbox.AWS.User.User                       as User
import qualified Flowbox.Data.Time                           as Time
import           Flowbox.Prelude                             hiding (error, use)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Control.Pool.Instance.Instance"


releaseUser :: EC2Resource m => User.Name -> MPool -> EC2 m ()
releaseUser userName mpool = do
    logger info $ "Releasing instances of user " ++ show userName
    instances <- Pool.findInstances (Just userName)
    mapM_ (flip release mpool) (map Types.instanceId instances)


release :: EC2Resource m => Instance.ID -> MPool -> EC2 m ()
release instanceID mpool = do
    logger info $ "Releasing instance " ++ show instanceID
    liftIO $ MVar.modifyMVar_ mpool (\pool -> do
        logger trace $ "Pool: " ++ show pool
        return $ Pool.free instanceID pool)
    Tag.tag [Tag.userTag Nothing] [instanceID] -- TODO [PM] : noUser = Nothing


retrieve :: EC2Resource m => User.Name -> Types.RunInstancesRequest -> MPool -> EC2 m Types.Instance
retrieve userName instanceRequest mpool = do
    logger info $ "Retreiving instance for user " ++ show userName
    poolEntry <- liftIO $ MVar.modifyMVar mpool (\pool -> do
        logger trace $ "Pool: " ++ show pool
        currentTime <- Time.getCurrentTime
        case Pool.getUsed userName pool of
            used:_ -> return (pool, Just used)
            []     -> case Pool.sortBySpareSeconds currentTime $ Pool.getFree pool of
                           f:_ -> return (Pool.use userName (fst f) pool, Just f)
                           []  -> return (pool, Nothing))
    instanceID <- case poolEntry of
        Just (instanceID, InstanceInfo _ InstanceState.Free) -> do
            logger info $ "Reusing instance " ++ show instanceID
            Tag.tag [Tag.userTag $ Just userName] [instanceID]
            prepareForNewUser userName instanceID
            return instanceID
        Just (instanceID, InstanceInfo _ (InstanceState.Used _)) -> do
            logger info $ "User instance already started " ++ show instanceID
            return instanceID
        Nothing -> do
            currentTime <- liftIO $ Time.getCurrentTime
            let tags = (Tag.poolKey, Tag.poolValue)
                     : Tag.startTimeTag currentTime
                     : [Tag.userTag $ Just userName]
            [inst] <- Management.startNewWait instanceRequest tags
            let instanceID = Types.instanceId inst
            liftIO $ MVar.modifyMVar_ mpool (\pool -> do
                logger trace $ "Pool: " ++ show pool
                return $ Map.insert instanceID (InstanceInfo currentTime $ InstanceState.Used userName) pool)
            return instanceID
    Management.byID instanceID


prepareForNewUser :: EC2Resource m => User.Name -> Instance.ID -> EC2 m ()
prepareForNewUser userName instanceID =
    logger warning "Prepare instance - not implemented"


getCandidatesToShutdown :: Time.UTCTime -> Pool -> [Pool.PoolEntry]
getCandidatesToShutdown currentTime = Map.toList . Map.filter isCandidateToShutdown where
    isCandidateToShutdown info' = (info' ^. InstanceInfo.state == InstanceState.Free)
                               && (InstanceInfo.spareSeconds info' currentTime > Pool.shutDownDiff)


freeUnused :: EC2Resource m => MPool -> EC2 m ()
freeUnused mpool = do
    logger debug $ "Terminating unused instances..."
    candidates <- liftIO $ MVar.modifyMVar mpool (\pool -> do
        logger trace $ "Pool: " ++ show pool
        currentTime <- Time.getCurrentTime
        let candidates = map fst $ getCandidatesToShutdown currentTime pool
            newPool    = foldr Map.delete pool candidates
        return (newPool, candidates))
    if length candidates > 0
        then do void $ EC2.terminateInstances candidates
                logger info $ "Terminating unused instances - terminated " ++ (show $ length candidates)
        else logger debug "Terminating unused instances - nothing to do"


monitor :: EC2Resource m => MPool -> EC2 m ()
monitor mpool = do
    logger info "Instance monitoring started"
    forever $ do
        let seconds = 30
        freeUnused mpool
        logger debug $ "Instance monitoring - sleeping " ++ show seconds ++ " seconds"
        liftIO $ Concurrent.threadDelay (seconds*1000*1000)
