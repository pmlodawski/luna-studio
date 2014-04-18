---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Config (
    module Paths_flowbox_account_manager,

    defaultAddress,
    defaultRegion,
) where

import Flowbox.Prelude
import Paths_flowbox_account_manager (version)



defaultAddress :: String
defaultAddress = "tcp://*:30530"


defaultRegion :: String
defaultRegion = "eu-west-1"
