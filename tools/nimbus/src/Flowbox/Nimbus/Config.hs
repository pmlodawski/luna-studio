---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Nimbus.Config (
    module Paths_flowbox_nimbus,
    Config(..),
) where

import Flowbox.Prelude
import Paths_flowbox_nimbus (version)



data Config = Config { ami            :: String 
                     , machine        :: String
                     , region         :: String
                     , credentialPath :: FilePath
                     }

instance Default Config where
    def = Config { ami            = "ami-a921dfde"
                 , machine        = "t1.micro"
                 , region         = "eu-west-1"
                 , credentialPath = "aws.config"
                 }
