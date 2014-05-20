---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.EC2.Control.Pool.Instance.State where

import qualified Flowbox.AWS.User.User as User
import           Flowbox.Prelude



data InstanceState = Free
                   | Used User.Name
                   deriving (Eq, Ord, Show, Read)
