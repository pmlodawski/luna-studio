---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.EC2.Instance.WaitTime where

import Flowbox.Prelude



data WaitTimes = WaitTimes { initial     :: Int
                           , next        :: Int
                           , repeatCount :: Int
                           } deriving (Show, Read, Eq, Ord)


instance Default WaitTimes where
    def = WaitTimes { initial = 10000000
                    , next    = 10000000
                    , repeatCount = 100
                    }
