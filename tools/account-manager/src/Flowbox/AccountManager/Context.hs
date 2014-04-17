---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Context where

import qualified AWS        as AWS
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.AWS.Region        (Region)
import           Flowbox.AWS.User.Database (Database)
import qualified Flowbox.AWS.User.Database as Database
import           Flowbox.Prelude           hiding (Context)



type ContextRef = IORef Context


data Context = Context { database   :: Database
                       , credential :: AWS.Credential
                       , region     :: Region
                       } deriving (Show)


mk :: Region -> IO ContextRef
mk region' = do credential' <- AWS.loadCredential
                IORef.newIORef $ Context Database.empty credential' region'
