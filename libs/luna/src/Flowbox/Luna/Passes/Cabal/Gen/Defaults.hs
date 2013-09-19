---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Cabal.Gen.Defaults where

import           Flowbox.Prelude                         
import           Flowbox.Luna.Passes.Cabal.CabalConfig   (CabalConfig(CabalConfig))
import qualified Flowbox.Luna.Passes.Cabal.CabalModule as CabalModule
import           Flowbox.Luna.Passes.Cabal.CabalModule   (CabalModule)
import           Flowbox.Luna.Passes.Cabal.Section       (Section(Section))
import qualified Flowbox.System.UniPath                as UniPath
import           Flowbox.System.UniPath                  (UniPath)


defaultConfig :: String -> [UniPath] -> UniPath -> CabalConfig
defaultConfig name hsSourceDirs mainIs = 
    CabalConfig [ Section "Name"          [name]
                , Section "Version"       ["0.1"]
                , Section "Cabal-Version" [">= 1.8"]
                , Section "Build-Type"    ["Simple"]
                ] [defaultModule name hsSourceDirs mainIs]

defaultModule :: String -> [UniPath] -> UniPath -> CabalModule
defaultModule name hsSourceDirs mainIs = 
    CabalModule.Executable name [ Section "Hs-Source-Dirs" $ map UniPath.toUnixString hsSourceDirs 
                                , Section "GHC-Options"    ["-Wall -O2"]
                                , Section "Extensions"     []
                                , Section "Build-Depends"  ["base"]
                                , Section "Main-Is"        [UniPath.toUnixString mainIs]
                                ]