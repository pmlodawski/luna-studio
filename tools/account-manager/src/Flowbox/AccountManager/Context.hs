---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Context where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.AWS.User.Database (Database)
import qualified Flowbox.AWS.User.Database as Database
import           Flowbox.Prelude           hiding (Context)



type Context = IORef Database


mk :: IO Context
mk = IORef.newIORef Database.empty
