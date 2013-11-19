---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flowbox.Lunac.Builder where

import           Control.Applicative                     

import           Flowbox.Prelude                         
--import           Flowbox.Config.Config                   (Config)
import qualified Flowbox.Luna.Passes.General.Luna.Luna as Luna

import qualified Flowbox.Luna.Passes.Build.Build       as Build
import qualified Flowbox.Luna.Passes.Build.BuildConfig as BuildConfig
import           Flowbox.Luna.Passes.Build.BuildConfig   (BuildConfig(BuildConfig))
--import qualified Flowbox.Lunac.CmdArgs                 as CmdArgs
--import           Flowbox.Lunac.CmdArgs                   (CmdArgs)
import           Flowbox.Luna.Passes.Build.Diagnostics   (Diagnostics)
import           Flowbox.System.Log.Logger               
import qualified Flowbox.System.UniPath                as UniPath
import           Flowbox.System.UniPath                  (UniPath)



logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder"


--build :: Config -> CmdArgs -> Diagnostics -> UniPath -> IO ()
build cfg cmd diag filePath = Luna.runIO $ do 
    --let name       = CmdArgs.libName    cmd
    --    version    = CmdArgs.libVersion cmd
    --    rootPath   = case CmdArgs.rootPath cmd of 
    --                    "" -> UniPath.basePath filePath
    --                    rp -> UniPath.fromUnixString rp
    --    libs       = CmdArgs.link cmd
    --ghcFlags <- case CmdArgs.optimisation cmd of
    --                0 -> pure ["-O0"]
    --                1 -> pure ["-O1"]
    --                2 -> pure ["-O2"]
    --                _ -> fail "Unsupported optimisation level"
    --let cabalFlags = case CmdArgs.global cmd of 
    --                    True  -> ["--global"]
    --                    False -> []
    --    outputPath = UniPath.fromUnixString $ CmdArgs.output cmd
    --    buildType  = if CmdArgs.library cmd 
    --                    then BuildConfig.Library 
    --                    else BuildConfig.Executable outputPath 
    --    bldCfg = BuildConfig name version libs ghcFlags cabalFlags buildType cfg diag
    --ast  <- Build.parseFile rootPath filePath
    --Build.run bldCfg ast 
    return ()
