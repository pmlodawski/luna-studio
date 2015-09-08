---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.InstanceManager.Config (
    module Paths_flowbox_instance_manager,
    Config(..),
) where

import Flowbox.Prelude
import Paths_flowbox_instance_manager (version)



data Config = Config { ami            :: String
                     , machine        :: String
                     , region         :: String
                     , credentialPath :: FilePath
                     , keyName        :: String
                     }


instance Default Config where
    def = Config { ami            = "ami-735da304"
                 , machine        = "g2.2xlarge"
                 , region         = "eu-west-1"
                 , credentialPath = "aws.config"
                 , keyName        = "nimbus"
                 }
