---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.EC2.Control.DBPool.Cost where

import           Flowbox.AWS.Region (Region)
import qualified Flowbox.AWS.Region as Region
import           Flowbox.Prelude



instanceHour :: Region -> Int
instanceHour region = case region of
    Region.AsiaPacific_Tokyo         -> 898
    Region.AsiaPacific_Singapore     -> 1000
    Region.AsiaPacific_Sydney        -> 898
    Region.EU_Ireland                -> 702
    Region.USEast_NorthernVirginia   -> 650
    Region.USWest_NorthernCalifornia -> 702
    Region.USWest_Oregon             -> 650
