---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.EC2.Control.DBPool.Cost where

import           Flowbox.AWS.User.User (User)
import qualified Flowbox.AWS.User.User as User
import           Flowbox.Prelude


instanceHour :: Int
instanceHour = 650
