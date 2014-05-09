---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Config (
    module Paths_flowbox_account_manager_mock,

    Config(..),
) where

import Flowbox.Prelude
import Paths_flowbox_account_manager_mock (version)



data Config = Config { address :: String
                     , region  :: String
                     }


instance Default Config where
    def = Config { address = "tcp://*:30530"
                 , region  = "eu-west-1"
                 }
