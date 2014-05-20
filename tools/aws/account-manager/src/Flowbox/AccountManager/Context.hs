---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Context where

import qualified AWS                        as AWS
import qualified Database.PostgreSQL.Simple as PSQL

import           Flowbox.AWS.EC2.Control.Pool.Pool (MPool)
import qualified Flowbox.AWS.EC2.Control.Pool.Pool as Pool
import qualified Flowbox.AWS.EC2.EC2               as EC2
import           Flowbox.AWS.Region                (Region)
import           Flowbox.AWS.User.Database         (Database)
import qualified Flowbox.AWS.User.Database         as Database
import           Flowbox.Prelude                   hiding (Context)



data Context = Context { database   :: Database
                       , pool       :: MPool
                       , credential :: AWS.Credential
                       , region     :: Region
                       }


mk :: Region -> PSQL.ConnectInfo -> IO Context
mk region' dbConnectionInfo = do
    credential' <- AWS.loadCredential
    database'   <- Database.mk dbConnectionInfo
    pool'       <- EC2.runEC2InRegion credential' region' $ Pool.initialize
    return $ Context database' pool' credential' region'
