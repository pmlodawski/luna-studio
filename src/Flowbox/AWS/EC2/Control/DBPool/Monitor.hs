---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.EC2.Control.DBPool.Monitor where

import qualified AWS                        as AWS
import qualified AWS.EC2.Types              as Types
import qualified Control.Concurrent         as Concurrent
import           Control.Monad              (forever, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.List                  ((\\))
import qualified Data.Time                  as Time
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.Instance         as InstanceDB
import qualified Flowbox.AWS.Database.Session          as SessionDB
import qualified Flowbox.AWS.Database.User             as UserDB
import qualified Flowbox.AWS.EC2.Control.DBPool.Cost   as Cost
import qualified Flowbox.AWS.EC2.Control.DBPool.Credit as Credit
import qualified Flowbox.AWS.EC2.Control.DBPool.Tag    as Tag
import           Flowbox.AWS.EC2.EC2                   (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2                   as EC2
import           Flowbox.AWS.EC2.Instance.Instance     (Instance (Instance))
import qualified Flowbox.AWS.EC2.Instance.Instance     as Instance
import qualified Flowbox.AWS.EC2.Instance.Management   as Management
import qualified Flowbox.AWS.EC2.Instance.Tag          as Tag
import           Flowbox.AWS.Region                    (Region)
import           Flowbox.AWS.User.Session              (Session)
import qualified Flowbox.AWS.User.Session              as Session
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger             hiding (error)



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Control.DBPool.Monitor"


nearEndGapTime :: Time.NominalDiffTime
nearEndGapTime = 5 * 60

extendByTime :: Time.NominalDiffTime
extendByTime = 60 * 60

shutDownDiff :: Int
shutDownDiff = 55*60 -- 55 min

---------------------------------------------------------------------------

nearToEnd :: Time.UTCTime -> Time.UTCTime -> Bool
nearToEnd current expires = Time.diffUTCTime expires current < nearEndGapTime


updateSession :: Region -> PSQL.Connection -> Session -> IO ()
updateSession region conn session = do
    let userName = session ^. Session.userName
        expires  = session ^. Session.expires
    user        <- UserDB.find conn userName
    currentTime <- Time.getCurrentTime
    case user of
        Nothing -> removeSession $ "User " ++ show userName ++ " no longer exists"
        Just u  -> if nearToEnd currentTime expires
            then case Credit.charge u $ Cost.instanceHour region of
                Left  msg         -> removeSession msg
                Right chargedUser -> do UserDB.update conn chargedUser
                                        extendSession
            else return ()
    where
        removeSession reason = do
            logger info $ "Removing session " ++ (show $ session ^. id) ++ " : " ++ reason
            SessionDB.deleteByID conn $ session ^. Session.id

        extendSession = do
            logger info $ "Extending session " ++ (show $ session ^. id)
            SessionDB.update conn $ session & Session.expires %~ Time.addUTCTime extendByTime


updateSessions :: Region -> PSQL.Connection -> IO ()
updateSessions region conn = PSQL.withTransaction conn
    (mapM_ (updateSession region conn) =<< SessionDB.all conn)



isShutDownCandidate :: Time.UTCTime -> Instance -> Bool
isShutDownCandidate currentTime inst =
    Instance.spareSeconds currentTime inst > shutDownDiff


freeUnusedInstances :: AWS.Credential -> Region -> PSQL.Connection -> IO ()
freeUnusedInstances credential region conn = do
    instancesToStop <- PSQL.withTransaction conn $ do
        currentTime <- Time.getCurrentTime
        free <- filter (isShutDownCandidate currentTime)
                <$> InstanceDB.findWithAtMostUsers conn 0
        mapM_ (InstanceDB.update conn) $ map (set Instance.status Instance.Stopped) free
        return $ map (view Instance.id) free
    if null instancesToStop
        then logger debug "Monitor : nothing to stop"
        else do logger info $ "Monitor : stopping " ++ (show $ length instancesToStop) ++ " instances"
                void $ EC2.runEC2inRegion credential region $ EC2.stopInstances instancesToStop True


detectOrphans :: AWS.Credential -> Region -> PSQL.Connection -> IO ()
detectOrphans credential region conn = do
    detectedInstances <- EC2.runEC2inRegion credential region $ do
        instances <- Management.findInstances $ Tag.filter Tag.poolKey [Tag.poolValue]
        mapM readInstanceData instances

    PSQL.withTransaction conn $ do
        existingInstances <- InstanceDB.all conn
        let orphanInstances   = detectedInstances \\ existingInstances
            unsyncedInstances = existingInstances \\ detectedInstances

        when (not $ null orphanInstances) $ do
            logger warning $ "Monitor : Detected " ++ (show $ length orphanInstances) ++ " orpan instances"
            mapM_ (InstanceDB.add conn) orphanInstances

        when (not $ null unsyncedInstances) $ do
            logger warning $ "Monitor : Detected " ++ (show $ length unsyncedInstances) ++ " unsynced instances"
            InstanceDB.delete conn $ map (view Instance.id) unsyncedInstances



readInstanceData :: EC2Resource m => Types.Instance -> EC2 m Instance
readInstanceData inst = do
    let instanceID = Types.instanceId inst
    startTime <- case Tag.getStartTime inst of
        Just time -> return time
        Nothing   -> do logger error $ "Failed to read instance " ++ show instanceID ++ " start time"
                        currentTime <- liftIO $ Time.getCurrentTime
                        Tag.tag [Tag.startTimeTag currentTime] [instanceID]
                        return currentTime
    let status = case Types.instanceState inst of
            Types.InstanceStateRunning -> Instance.Running
            Types.InstanceStateStopped -> Instance.Stopped
    return $ Instance instanceID startTime status


run :: AWS.Credential -> Region -> PSQL.Connection -> IO ()
run credential region conn = forever $ do
    detectOrphans       credential region conn
    updateSessions                 region conn
    freeUnusedInstances credential region conn
    Concurrent.threadDelay (60 * 1000 * 1000) -- sleep one minute

