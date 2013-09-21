---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Defaults where

import           Flowbox.Prelude                  
import qualified Flowbox.Luna.Data.Cabal.Config as Config
import           Flowbox.Luna.Data.Cabal.Config   (Config)
import qualified Flowbox.Luna.Data.Cabal.Module as Module
import           Flowbox.Luna.Data.Cabal.Module   (Module)
import qualified Flowbox.System.UniPath         as UniPath
import           Flowbox.System.UniPath           (UniPath)


defaultConfig :: String -> [UniPath] -> UniPath -> Config
defaultConfig name hsSourceDirs mainIs = 
    Config.make name 
                "0.1" 
                ">= 1.8" 
                "Simple" 
                [defaultModule name hsSourceDirs mainIs]

defaultModule :: String -> [UniPath] -> UniPath -> Module
defaultModule name hsSourceDirs mainIs = 
    Module.mkExecutable name 
                        (map UniPath.toUnixString hsSourceDirs) 
                        "-Wall -O2" 
                        []
                        ["base"]
                        (UniPath.toUnixString mainIs)