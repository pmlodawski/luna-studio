---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.EC2.Control.Pool.Pool where

import qualified AWS.EC2.Types           as Types
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.List               as List
import           Data.Map                (Map)
import qualified Data.Map                as Map

import           Flowbox.AWS.EC2.Control.Pool.Instance.Info  (InstanceInfo (InstanceInfo))
import qualified Flowbox.AWS.EC2.Control.Pool.Instance.Info  as InstanceInfo
import qualified Flowbox.AWS.EC2.Control.Pool.Instance.State as InstanceState
import qualified Flowbox.AWS.EC2.Control.Pool.Tag            as Tag
import           Flowbox.AWS.EC2.EC2                         (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.Instance.Instance           as Instance
import qualified Flowbox.AWS.EC2.Instance.Management         as Management
import qualified Flowbox.AWS.EC2.Instance.Tag                as Tag
import qualified Flowbox.AWS.User.User                       as User
import qualified Flowbox.Data.Time                           as Time
import           Flowbox.Prelude                             hiding (error)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Control.Pool.Pool"


type Pool = Map Instance.ID InstanceInfo


type PoolEntry = (Instance.ID, InstanceInfo)


type MPool = MVar Pool

----------------------------------------------------------

getUsed :: User.Name -> Pool -> [PoolEntry]
getUsed userName = Map.toList . Map.filter (\info' -> info' ^. InstanceInfo.state == InstanceState.Used userName)


free :: Instance.ID -> Pool -> Pool
free = Map.adjust (set InstanceInfo.state InstanceState.Free)


getFree :: Pool -> [PoolEntry]
getFree = Map.toList . Map.filter (\info' -> info' ^. InstanceInfo.state == InstanceState.Free)


use :: User.Name -> Instance.ID -> Pool -> Pool
use userName = Map.adjust (set InstanceInfo.state $ InstanceState.Used userName)


sortBySpareSeconds :: Time.UTCTime -> [PoolEntry] -> [PoolEntry]
sortBySpareSeconds currentTime = List.sortBy (\a b ->
    compare (InstanceInfo.spareSeconds (snd a) currentTime)
            (InstanceInfo.spareSeconds (snd b) currentTime))

----------------------------------------------------------

initialize :: EC2Resource m => EC2 m MPool
initialize = do
    logger info $ "Initializing instance pool"
    instances <- findInstances Nothing
    entries <- mapM readInstanceInfo instances
    let pool = Map.fromList entries
    logger trace $ "Pool: " ++ show pool
    liftIO $ MVar.newMVar pool


readInstanceInfo :: EC2Resource m => Types.Instance -> EC2 m PoolEntry
readInstanceInfo inst = do
    let instanceID = Types.instanceId inst
    startTime <- case Tag.getStartTime inst of
        Just time -> return time
        Nothing   -> do logger error $ "Failed to read instance " ++ show instanceID ++ " start time"
                        currentTime <- liftIO $ Time.getCurrentTime
                        Tag.tag [Tag.startTimeTag currentTime] [instanceID]
                        return currentTime
    let instanceState = case Tag.getUser inst of
            Just userName -> InstanceState.Used userName
            Nothing       -> InstanceState.Free
    return $ (instanceID, InstanceInfo startTime instanceState)


findInstances :: EC2Resource m => Maybe User.Name -> EC2 m [Types.Instance]
findInstances userName = do
    let f = (Tag.filter Tag.poolKey [Tag.poolValue]) ++ case userName of
                                                         Just user -> Tag.userFilter user
                                                         Nothing   -> []
    Management.findInstances f


shutDownDiff :: Int
shutDownDiff = 55*60 -- 55 min

