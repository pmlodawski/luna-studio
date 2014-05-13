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
import qualified AWS.EC2.Util            as Util
import qualified Control.Concurrent      as Concurrent
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Data.Text               as Text

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2               as EC2
import qualified Flowbox.AWS.EC2.Instance.ID       as Instance
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Instance.Tag      as Tag
import qualified Flowbox.AWS.User.User             as User
import qualified Flowbox.Data.Time                 as Time
import           Flowbox.Prelude                   hiding (error, use)
import           Flowbox.System.Log.Logger



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
getUsed userName = Map.toList . Map.filter (\info' -> info' ^. state == Used userName)


free :: Instance.ID -> Pool -> Pool
free = Map.adjust (set state Free)


getFree :: Pool -> [PoolEntry]
getFree = Map.toList . Map.filter (\info' -> info' ^. state == Free)


use :: User.Name -> Instance.ID -> Pool -> Pool
use userName = Map.adjust (set state $ Used userName)


shutDownDiff :: Int
shutDownDiff = 55*60 -- 55 min


getCandidatesToShutdown :: Time.UTCTime -> Pool -> [PoolEntry]
getCandidatesToShutdown currentTime = Map.toList . Map.filter isCandidateToShutdown where
    isCandidateToShutdown info' =
        (Time.toSeconds $ Time.diffUTCTime currentTime $ info' ^. started) `mod` 3600 > shutDownDiff

----------------------------------------------------------

poolTagKey :: Tag.TagKey
poolTagKey = Text.pack "pool"


poolTagValue :: Tag.TagValue
poolTagValue = Text.pack "1.0"


findPoolInstances :: EC2Resource m => EC2 m [Types.Instance]
findPoolInstances =
    concatMap Types.reservationInstanceSet <$> (Util.list $ EC2.describeInstances [] $ Tag.filter poolTagKey [poolTagValue])


readInstanceInfo :: EC2Resource m => Types.Instance -> EC2 m PoolEntry
readInstanceInfo inst = do
    let instanceID = Types.instanceId inst
    startTime <- case Tag.getStartTime inst of
        Just time -> return time
        Nothing   -> do logger error $ "Failed to read instance " ++ show instanceID ++ " start time"
                        currentTime <- liftIO $ Time.getCurrentTime
                        Tag.tagWithStartTime currentTime [instanceID]
                        return currentTime
    let instanceState = case Tag.getUser inst of
            Just userName -> Used userName
            Nothing       -> Free
    return $ (instanceID, InstanceInfo startTime instanceState)

----------------------------------------------------------

initialize :: EC2Resource m => EC2 m MPool
initialize = do
    logger info $ "Initializing instance pool"
    instances <- findPoolInstances
    entries <- mapM readInstanceInfo instances
    liftIO $ MVar.newMVar $ Map.fromList entries


release :: EC2Resource m => Instance.ID -> MPool -> EC2 m ()
release instanceID mpool = do
    logger info $ "Releasing instance " ++ show instanceID
    liftIO $ MVar.modifyMVar_ mpool (return . free instanceID)
    Tag.tagWithUser Nothing [instanceID]
    freeUnused mpool


retrieve :: EC2Resource m => User.Name -> Types.RunInstancesRequest -> MPool -> EC2 m Instance.ID
retrieve userName instanceRequest mpool = do
    logger info $ "Retreiving instance for user " ++ show userName
    poolEntry <- liftIO $ MVar.modifyMVar mpool (\pool ->
        case getUsed userName pool of
            used:_ -> return (pool, Just used)
            []     -> case getFree pool of
                           f:_ -> return (use userName (fst f) pool, Just f)
                           []  -> return (pool, Nothing))
    case poolEntry of
        Just (instanceID, InstanceInfo _ Free) -> do
            logger info $ "Reusing instance " ++ show instanceID
            Tag.tagWithUser (Just userName) [instanceID]
            Instance.prepareForNewUser userName instanceID
            return instanceID
        Just (instanceID, InstanceInfo _ (Used _)) -> do
            logger info $ "User instance already started " ++ show instanceID
            return instanceID
        Nothing -> do
            logger info $ "Starting new instance"
            inst <- Instance.startNew userName instanceRequest
            Tag.tag poolTagKey poolTagValue [Types.instanceId inst]
            time <- fromJust $ Tag.getStartTime inst
            let instanceID = Types.instanceId inst
            liftIO $ MVar.modifyMVar_ mpool (return . Map.insert instanceID (InstanceInfo time $ Used userName))
            return instanceID


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
        else logger debug $ "Releasing unused instances - nothing to do " ++ (show $ length candidates)


monitor :: EC2Resource m => MPool -> EC2 m ()
monitor mpool = do
    let seconds = 60
    logger debug $ "Instance monitoring - sleeping " ++ show seconds ++ " seconds"
    liftIO $ Concurrent.threadDelay (seconds*1000*1000)
    freeUnused mpool
