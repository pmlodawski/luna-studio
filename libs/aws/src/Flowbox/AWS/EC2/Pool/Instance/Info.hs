---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AWS.EC2.Pool.Instance.Info where

import           Flowbox.AWS.EC2.Pool.Instance.State (InstanceState)
import qualified Flowbox.Data.Time                   as Time
import           Flowbox.Prelude                     hiding (error, use)



data InstanceInfo = InstanceInfo { _started :: Time.UTCTime
                                 , _state   :: InstanceState
                                 } deriving (Eq, Ord, Show, Read)


makeLenses(''InstanceInfo)
