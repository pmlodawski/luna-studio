---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Context where

import qualified AWS                        as AWS
import qualified Database.PostgreSQL.Simple as PSQL

import           Flowbox.AWS.Region                 (Region)
import           Flowbox.AWS.User.Database.Database (Database)
import qualified Flowbox.AWS.User.Database.Database as Database
import           Flowbox.Prelude                    hiding (Context)



data Context = Context { database   :: Database
                       , credential :: AWS.Credential
                       , region     :: Region
                       }


mk :: Region -> PSQL.ConnectInfo -> IO Context
mk region' dbConnectionInfo = do
    credential' <- AWS.loadCredential
    database'   <- Database.mk dbConnectionInfo
    return $ Context database' credential' region'
