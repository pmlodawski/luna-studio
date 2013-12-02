---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Lunac.Version  where

import qualified Data.Version               as Version

import           Flowbox.Prelude              
import qualified Flowbox.Luna.Config.Config as LibConfig
import qualified Flowbox.Lunac.Config       as Config



full :: Bool -> Bool -> Bool -> String
full numeric comp lib =  (if (not comp && lib) then "" else (compiler numeric ++ "\n")) 
                      ++ (if (not lib && comp) then "" else (library numeric ++ "\n")) 


compiler :: Bool -> String
compiler numeric = (if numeric then "" else "Luna compiler version ") ++ Version.showVersion Config.version


library :: Bool -> String
library numeric = (if numeric then "" else "Luna library version ") ++ Version.showVersion LibConfig.version
