---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Flowbox.AWS.EC2.Instance.Pool where

import qualified AWS.EC2.Types           as Types
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Control.Concurrent as Concurrent

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2               as EC2
import qualified Flowbox.AWS.EC2.Instance.ID       as Instance
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag      as Tag
import qualified Flowbox.AWS.User.User             as User
import qualified Flowbox.Data.Time                 as Time
import           Flowbox.Prelude                   hiding (use)
import           Flowbox.System.Log.Logger         hiding (info)



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Instance.Pool"


data InstanceState = Free
                   | Used User.Name
                   deriving (Eq, Ord, Show, Read)


data InstanceInfo = InstanceInfo { _started :: Time.UTCTime
                                 , _state   :: InstanceState
                                 } deriving (Eq, Ord, Show, Read)


makeLenses(''InstanceInfo)


type Pool = Map Instance.ID InstanceInfo


type PoolEntry = (Instance.ID, InstanceInfo)


type MPool = MVar Pool

----------------------------------------------------------

getUsed :: User.Name -> Pool -> [PoolEntry]
getUsed userName = Map.toList . Map.filter (\info -> info ^. state == Used userName)


free :: Instance.ID -> Pool -> Pool
free = Map.adjust (set state Free)


getFree :: Pool -> [PoolEntry]
getFree = Map.toList . Map.filter (\info -> info ^. state == Free)


use :: User.Name -> Instance.ID -> Pool -> Pool
use userName = Map.adjust (set state $ Used userName)


shutDownDiff :: Int
shutDownDiff = 55*60 -- 55 min


getCandidatesToShutdown :: Time.UTCTime -> Pool -> [PoolEntry]
getCandidatesToShutdown currentTime = Map.toList . Map.filter isCandidateToShutdown where
    isCandidateToShutdown info =
        (Time.toSeconds $ Time.diffUTCTime currentTime $ info ^. started) `mod` 3600 > shutDownDiff

----------------------------------------------------------

initialize :: EC2Resource m => EC2 m ()
initialize = undefined


release :: EC2Resource m => Instance.ID -> MPool -> EC2 m ()
release instanceID mpool = do
    liftIO $ MVar.modifyMVar_ mpool (return . free instanceID)
    Tag.tagWithUser Nothing [instanceID]
    freeUnused mpool


retrieve :: EC2Resource m => User.Name -> Types.RunInstancesRequest -> MPool -> EC2 m Instance.ID
retrieve userName instanceRequest mpool = do
    poolEntry <- liftIO $ MVar.modifyMVar mpool (\pool ->
        case getUsed userName pool of
            used:_ -> return (pool, Just used)
            []     -> case getFree pool of
                           f:_ -> return (use userName (fst f) pool, Just f)
                           []  -> return (pool, Nothing))
    case poolEntry of
        Just (instanceID, InstanceInfo _ Free) -> do
            Tag.tagWithUser (Just userName) [instanceID]
            Instance.prepareForNewUser userName instanceID
            return instanceID
        Just (instanceID, InstanceInfo _ (Used _)) -> do
            return instanceID
        Nothing -> do
            inst <- Instance.startNew userName instanceRequest
            time <- fromJust $ Tag.getStartTime inst
            let instanceID = Types.instanceId inst
            liftIO $ MVar.modifyMVar_ mpool (return . Map.insert instanceID (InstanceInfo time $ Used userName))
            return instanceID


freeUnused :: EC2Resource m => MPool -> EC2 m ()
freeUnused mpool = do
    candidates <- liftIO $ MVar.modifyMVar mpool (\pool -> do
        currentTime <- Time.getCurrentTime
        let candidates = map fst $ getCandidatesToShutdown currentTime pool
            newPool    = foldr Map.delete pool candidates
        return (newPool, candidates))
    void $ EC2.terminateInstances candidates



monitor :: EC2Resource m => MPool -> EC2 m ()
monitor mpool = do 
    liftIO $ Concurrent.threadDelay (60*1000*1000) -- 1min
    freeUnused mpool
