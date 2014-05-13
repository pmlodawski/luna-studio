---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Flowbox.AWS.EC2.Pool.Instance.Instance where

import qualified AWS.EC2.Types           as Types
import qualified Control.Concurrent      as Concurrent
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map                as Map

import           Flowbox.AWS.EC2.EC2                 (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2                 as EC2
import qualified Flowbox.AWS.EC2.Instance.ID         as Instance
import qualified Flowbox.AWS.EC2.Instance.Instance   as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag        as Tag
import           Flowbox.AWS.EC2.Pool.Instance.Info  (InstanceInfo (InstanceInfo))
import qualified Flowbox.AWS.EC2.Pool.Instance.Info  as InstanceInfo
import qualified Flowbox.AWS.EC2.Pool.Instance.State as InstanceState
import           Flowbox.AWS.EC2.Pool.Pool           (MPool, Pool)
import qualified Flowbox.AWS.EC2.Pool.Pool           as Pool
import qualified Flowbox.AWS.EC2.Pool.Tag            as Tag
import qualified Flowbox.AWS.User.User               as User
import qualified Flowbox.Data.Time                   as Time
import           Flowbox.Prelude                     hiding (error, use)
import           Flowbox.System.Log.Logger




logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Pool.Instance.Instance"



release :: EC2Resource m => Instance.ID -> MPool -> EC2 m ()
release instanceID mpool = do
    logger info $ "Releasing instance " ++ show instanceID
    liftIO $ MVar.modifyMVar_ mpool (return . Pool.free instanceID)
    Tag.tagWithUser Nothing [instanceID]
    freeUnused mpool


retrieve :: EC2Resource m => User.Name -> Types.RunInstancesRequest -> MPool -> EC2 m Instance.ID
retrieve userName instanceRequest mpool = do
    logger info $ "Retreiving instance for user " ++ show userName
    poolEntry <- liftIO $ MVar.modifyMVar mpool (\pool ->
        case Pool.getUsed userName pool of
            used:_ -> return (pool, Just used)
            []     -> case Pool.getFree pool of
                           f:_ -> return (Pool.use userName (fst f) pool, Just f)
                           []  -> return (pool, Nothing))
    case poolEntry of
        Just (instanceID, InstanceInfo _ InstanceState.Free) -> do
            logger info $ "Reusing instance " ++ show instanceID
            Tag.tagWithUser (Just userName) [instanceID]
            Instance.prepareForNewUser userName instanceID
            return instanceID
        Just (instanceID, InstanceInfo _ (InstanceState.Used _)) -> do
            logger info $ "User instance already started " ++ show instanceID
            return instanceID
        Nothing -> do
            logger info $ "Starting new instance"
            inst <- Instance.startNew userName instanceRequest
            Tag.tag Tag.poolTagKey Tag.poolTagValue [Types.instanceId inst]
            time <- fromJust $ Tag.getStartTime inst
            let instanceID = Types.instanceId inst
            liftIO $ MVar.modifyMVar_ mpool (return . Map.insert instanceID (InstanceInfo time $ InstanceState.Used userName))
            return instanceID


getCandidatesToShutdown :: Time.UTCTime -> Pool -> [Pool.PoolEntry]
getCandidatesToShutdown currentTime = Map.toList . Map.filter isCandidateToShutdown where
    isCandidateToShutdown info' =
        (Time.toSeconds $ Time.diffUTCTime currentTime $ info' ^. InstanceInfo.started) `mod` 3600 > Pool.shutDownDiff


freeUnused :: EC2Resource m => MPool -> EC2 m ()
freeUnused mpool = do
    logger debug $ "Releasing unused instances..."
    candidates <- liftIO $ MVar.modifyMVar mpool (\pool -> do
        currentTime <- Time.getCurrentTime
        let candidates = map fst $ getCandidatesToShutdown currentTime pool
            newPool    = foldr Map.delete pool candidates
        return (newPool, candidates))
    if length candidates > 0
        then do logger info $ "Releasing unused instances - found " ++ (show $ length candidates)
                void $ EC2.terminateInstances candidates
                logger info $ "Releasing unused instances - terminated " ++ (show $ length candidates)
        else logger debug "Releasing unused instances - nothing to do"


monitor :: EC2Resource m => MPool -> EC2 m ()
monitor mpool = do
    let seconds = 60
    logger debug $ "Instance monitoring - sleeping " ++ show seconds ++ " seconds"
    liftIO $ Concurrent.threadDelay (seconds*1000*1000)
    freeUnused mpool
