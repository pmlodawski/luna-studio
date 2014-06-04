---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.AWS.EC2.Control.DBPool.Instance where

import qualified AWS                                    as AWS
import qualified AWS.EC2.Types                          as Types
import           Control.Monad                          (when)
import qualified Data.List                              as List
import qualified Data.Time                              as Time
import qualified Database.PostgreSQL.Simple             as PSQL
import qualified Database.PostgreSQL.Simple.Transaction as Transaction

import qualified Flowbox.AWS.Database.Instance         as InstanceDB
import qualified Flowbox.AWS.Database.Session          as SessionDB
import qualified Flowbox.AWS.Database.User             as UserDB
import qualified Flowbox.AWS.EC2.Control.DBPool.Cost   as Cost
import qualified Flowbox.AWS.EC2.Control.DBPool.Credit as Credit
import qualified Flowbox.AWS.EC2.Control.DBPool.Tag    as Tag
import qualified Flowbox.AWS.EC2.EC2                   as EC2
import           Flowbox.AWS.EC2.Instance.Instance     (Instance (Instance))
import qualified Flowbox.AWS.EC2.Instance.Instance     as Instance
import qualified Flowbox.AWS.EC2.Instance.Management   as Management
import qualified Flowbox.AWS.EC2.Instance.Request      as Request
import qualified Flowbox.AWS.EC2.Instance.Tag          as Tag
import           Flowbox.AWS.Region                    (Region)
import qualified Flowbox.AWS.User.Session              as Session
import qualified Flowbox.AWS.User.User                 as User
import           Flowbox.Control.Error                 (assert, eitherStringToM, (<??>))
import           Flowbox.Prelude





retrieve :: PSQL.Connection -> AWS.Credential -> Region
         -> User.Name -> Types.RunInstancesRequest -> IO [Types.Instance]
retrieve conn credential region userName instancesRequest = do
    let amount = Types.runInstancesRequestMinCount instancesRequest
        runEC2 = EC2.runEC2inRegion credential region
    assert (amount == Types.runInstancesRequestMaxCount instancesRequest) "runInstancesRequestMinCount must be equal to runInstancesRequestMaxCount"

    (runningIDs, stoppedIDs, startingIDs, tags) <- Transaction.withTransaction conn $ do

        -- user credit -----------------

        user        <- UserDB.find conn userName <??> "Cannot find user " ++ show userName ++ " in database."
        chargedUser <- eitherStringToM $ Credit.charge user $ Cost.instanceHour region
        UserDB.update conn chargedUser

        -- tags ------------------------

        currentTime <- Time.getCurrentTime
        let tags = (Tag.poolKey, Tag.poolValue)
                 : [Tag.startTimeTag currentTime]

        -- instaces from database ------

        available <- InstanceDB.findAvailable conn userName
        let (running, stopped') = List.partition Instance.isRunning $ take amount available
            stopped = map (set Instance.status  Instance.Running)
                    $ map (set Instance.started currentTime) stopped'
        mapM_ (InstanceDB.update conn) stopped

        -- new instances ---------------

        let amountLeft = amount - length available
        startingInfo <- if amountLeft > 0
            then do let request = Request.setAmount amountLeft instancesRequest
                    runEC2 $ Management.startNew request tags
            else return []
        let instanceFromID instanceID = Instance instanceID currentTime Instance.Running
            starting   = map (instanceFromID . Types.instanceId) startingInfo
        mapM_ (InstanceDB.add conn) starting

        -- session ---------------------

        let ids = map $ view Instance.id
            runningIDs  = ids running
            stoppedIDs  = ids stopped
            startingIDs = ids starting

            expires    = Time.addUTCTime 3600 currentTime
            policy     = Session.Autocharge
            addSession instanceID = SessionDB.create  conn userName instanceID expires policy

        mapM_ addSession $ runningIDs ++ stoppedIDs ++ startingIDs

        --------------------------------
        return (runningIDs, stoppedIDs, startingIDs, tags)

    runEC2 $ do
        runningInfo <- if null runningIDs
                            then return []
                            else Management.byIDs runningIDs

        when (not $ null stoppedIDs) $ Management.startExisting stoppedIDs tags

        startedInfo <- if null $ startingIDs ++ stoppedIDs
                            then return []
                            else Management.waitForStart (startingIDs ++ stoppedIDs) def
        return $ runningInfo ++ startedInfo



release :: PSQL.Connection -> User.Name -> Instance.ID -> IO ()
release conn userName instanceID =
    SessionDB.delete conn userName instanceID


