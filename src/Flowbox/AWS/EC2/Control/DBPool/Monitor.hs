---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.EC2.Control.DBPool.Monitor where

import qualified AWS                        as AWS
import qualified Control.Concurrent         as Concurrent
import           Control.Monad              (forever)
import qualified Data.Time                  as Time
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.Instance         as InstanceDB
import qualified Flowbox.AWS.Database.Session          as SessionDB
import qualified Flowbox.AWS.Database.User             as UserDB
import qualified Flowbox.AWS.EC2.Control.DBPool.Cost   as Cost
import qualified Flowbox.AWS.EC2.Control.DBPool.Credit as Credit
import qualified Flowbox.AWS.EC2.EC2                   as EC2
import qualified Flowbox.AWS.EC2.Instance.Instance     as Instance
import           Flowbox.AWS.Region                    (Region)
import           Flowbox.AWS.User.Session              (Session)
import qualified Flowbox.AWS.User.Session              as Session
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.EC2.Instance.Instance"


nearEndGapTime :: Time.NominalDiffTime
nearEndGapTime = 5 * 60

extendByTime :: Time.NominalDiffTime
extendByTime = 60 * 60

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


freeUnusedInstances :: AWS.Credential -> Region -> PSQL.Connection -> IO ()
freeUnusedInstances credential region conn = do
    instancesToStop <- PSQL.withTransaction conn $ do
        free <- InstanceDB.findWithAtMostUsers conn 0
        let freeIDs = map (view Instance.id) free
        InstanceDB.delete conn freeIDs
        return freeIDs
    void $ EC2.runEC2inRegion credential region $ EC2.stopInstances instancesToStop True


run :: AWS.Credential -> Region -> PSQL.Connection -> IO ()
run credential region conn = forever $ do
    updateSessions region conn
    freeUnusedInstances credential region conn
    Concurrent.threadDelay (60 * 1000 * 1000) -- sleep one minute





