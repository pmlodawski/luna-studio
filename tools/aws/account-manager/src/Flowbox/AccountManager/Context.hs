---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AccountManager.Context where

import qualified AWS                        as AWS
import qualified Database.PostgreSQL.Simple as PSQL

import           Flowbox.AWS.Database.Database (Database)
import qualified Flowbox.AWS.Database.Database as Database
import           Flowbox.AWS.Region            (Region)
import           Flowbox.Prelude               hiding (Context)



data Context = Context { _database   :: Database
                       , _credential :: AWS.Credential
                       , _region     :: Region
                       }

makeLenses (''Context)


mk :: Region -> PSQL.ConnectInfo -> IO Context
mk region' dbConnectionInfo =
    Context <$> Database.mk dbConnectionInfo
            <*> AWS.loadCredential
            <*> pure region'
