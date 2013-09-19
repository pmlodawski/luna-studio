---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Cabal.Gen.Defaults where

import           Flowbox.Prelude                         
import qualified Flowbox.Luna.Passes.Cabal.CabalConfig as CabalConfig
import           Flowbox.Luna.Passes.Cabal.CabalConfig   (CabalConfig)
import qualified Flowbox.Luna.Passes.Cabal.CabalModule as CabalModule
import           Flowbox.Luna.Passes.Cabal.CabalModule   (CabalModule)
import qualified Flowbox.System.UniPath                as UniPath
import           Flowbox.System.UniPath                  (UniPath)


defaultConfig :: String -> [UniPath] -> UniPath -> CabalConfig
defaultConfig name hsSourceDirs mainIs = 
    CabalConfig.make name 
                     "0.1" 
                     ">= 1.8" 
                     "Simple" 
                     [defaultModule name hsSourceDirs mainIs]

defaultModule :: String -> [UniPath] -> UniPath -> CabalModule
defaultModule name hsSourceDirs mainIs = 
    CabalModule.mkExecutable name 
                             (map UniPath.toUnixString hsSourceDirs) 
                             "-Wall -O2" 
                             []
                             ["base"]
                             (UniPath.toUnixString mainIs)