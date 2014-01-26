---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Build.BuildConfig where

import Data.Version (Version)

import Flowbox.Config.Config                 (Config)
import Flowbox.Luna.Passes.Build.Diagnostics (Diagnostics)
import Flowbox.Prelude
import Flowbox.System.UniPath                (UniPath)



data BuildConfig = BuildConfig { name       :: String
                               , version    :: Version
                               , libs       :: [String]
                               , ghcflags   :: [String]
                               , cppflags   :: [String]
                               , cabalflags :: [String]
                               , buildType  :: BuildType
                               , config     :: Config
                               , diag       :: Diagnostics
                               , buildDir   :: Maybe UniPath
                               }

data BuildType   = Executable  { outputPath :: UniPath }
                 | Library     { }
