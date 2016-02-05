---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Build.BuildConfig where

import           Data.Version           (Version)

import           Flowbox.Config.Config  (Config)
import           Flowbox.Prelude
import           Flowbox.System.UniPath (UniPath)
import           Luna.Build.Diagnostics (Diagnostics)



data BuildConfig = BuildConfig { _name        :: String
                               , _version     :: Version
                               , _libs        :: [String]
                               , _ghcflags    :: [String]
                               , _ccFlags     :: [String]
                               , _includeDirs :: [String]
                               , _cabalflags  :: [String]
                               , _buildType   :: BuildType
                               , _config      :: Config
                               , _diag        :: Diagnostics
                               , _buildDir    :: Maybe UniPath
                               , _includeStd  :: Bool
                               }

data BuildType   = Executable  { outputPath :: UniPath }
                 | Library


makeLenses ''BuildConfig
