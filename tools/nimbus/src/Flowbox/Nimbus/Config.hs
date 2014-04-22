---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Nimbus.Config (
    module Paths_flowbox_nimbus,
    defaultAmi,
    defaultMachine,
    defaultRegion,
    defaultCredentialFilePath,
) where

import Flowbox.Prelude
import Paths_flowbox_nimbus (version)



defaultAmi :: String
defaultAmi = "ami-a921dfde"


defaultMachine :: String
defaultMachine = "t1.micro"


defaultRegion :: String
defaultRegion = "eu-west-1"


defaultCredentialFilePath :: FilePath
defaultCredentialFilePath = "aws.config"
