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
retrieve connection credential region userName instancesRequest = do
    let amount = Types.runInstancesRequestMinCount instancesRequest
    assert (amount == Types.runInstancesRequestMaxCount instancesRequest) "runInstancesRequestMinCount must be equal to runInstancesRequestMaxCount"

    instanceIDs <- Transaction.withTransaction connection $ do
        user        <- UserDB.find connection userName <??> "Cannot find user " ++ show userName ++ " in database."
        updatedUser <- eitherStringToM $ Credit.charge user Cost.instanceHour
        UserDB.update connection updatedUser
        free        <- InstanceDB.findFree connection
        currentTime <- Time.getCurrentTime
        let amountLeft = amount - length free
            request    = Request.setAmount amountLeft instancesRequest
            tags       = (Tag.poolKey, Tag.poolValue)
                       : [Tag.startTimeTag currentTime]
            expires    = Time.addUTCTime 3600 currentTime
            policy     = Session.Autocharge
        justStarted <- EC2.runEC2InRegion credential region
                        $ Management.startNew request tags

        let instanceIDs = map (view Instance.id) free
                       ++ map Types.instanceId   justStarted
        mapM_ (\instanceID -> SessionDB.create connection userName instanceID expires policy)
              instanceIDs
        return instanceIDs

    EC2.runEC2InRegion credential region $ Management.waitForStart instanceIDs def


release :: PSQL.Connection -> User.Name -> Instance.ID -> IO ()
release connection userName instanceID =
    SessionDB.delete connection userName instanceID


