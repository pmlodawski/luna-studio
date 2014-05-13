---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.InstanceMonitor where

import           Flowbox.AccountManager.Context         (Context)
import qualified Flowbox.AccountManager.Context         as Context
import qualified Flowbox.AWS.EC2.EC2                    as EC2
import qualified Flowbox.AWS.EC2.Pool.Instance.Instance as Instance
import           Flowbox.Prelude                        hiding (Context)


run :: Context -> IO ()
run ctx = EC2.runEC2InRegion (Context.credential ctx) (Context.region ctx)
            $ Instance.monitor (Context.pool ctx)
