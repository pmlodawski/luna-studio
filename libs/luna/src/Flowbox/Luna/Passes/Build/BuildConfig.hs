---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Build.BuildConfig where

import           Flowbox.Prelude                         
import           Flowbox.Config.Config                   (Config)
import           Flowbox.Luna.Passes.Build.Diagnostics   (Diagnostics)
import           Flowbox.System.UniPath                  (UniPath)



data BuildConfig = BuildConfig { name       :: String
                               , version    :: String 
                               , libs       :: [String]
                               , ghcflags   :: [String]
                               , cabalflags :: [String]
                               , buildType  :: BuildType
                               , config     :: Config
                               , diag       :: Diagnostics
                               }

data BuildType   = Executable  { outputPath :: UniPath }
                 | Library     { } 
