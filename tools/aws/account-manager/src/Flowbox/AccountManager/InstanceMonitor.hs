---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.InstanceMonitor where

import           Flowbox.AccountManager.Context         (Context)
import qualified Flowbox.AccountManager.Context         as Context
import qualified Flowbox.AWS.EC2.Control.DBPool.Monitor as Monitor
import           Flowbox.Prelude                        hiding (Context)



run :: Context -> IO ()
run ctx = Monitor.run (ctx ^. Context.credential)
                      (ctx ^. Context.region)
                      (ctx ^. Context.database)
