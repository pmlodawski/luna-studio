---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Lunac.Build where

import Control.Applicative

import           Flowbox.Config.Config                 (Config)
import qualified Flowbox.Initializer.Initializer       as Initializer
import qualified Flowbox.Luna.Passes.Build.Build       as Build
import           Flowbox.Luna.Passes.Build.BuildConfig (BuildConfig (BuildConfig))
import qualified Flowbox.Luna.Passes.Build.BuildConfig as BuildConfig
import           Flowbox.Luna.Passes.Build.Diagnostics (Diagnostics (Diagnostics))
import qualified Flowbox.Luna.Passes.General.Luna.Luna as Luna
import qualified Flowbox.Lunac.Cmd                     as Cmd
import           Flowbox.Prelude                       hiding (op)
import           Flowbox.System.Log.Logger
import           Flowbox.System.UniPath                (UniPath)
import qualified Flowbox.System.UniPath                as UniPath



logger :: Logger
logger = getLogger "Flowbox.Lunac.Build"



run :: Config -> Cmd.Options -> IO ()
run cfg op = do
    let diag = Diagnostics False
                         ( Cmd.dump_ast  op || Cmd.dump_all op )
                         ( Cmd.dump_va   op || Cmd.dump_all op )
                         ( Cmd.dump_fp   op || Cmd.dump_all op )
                         ( Cmd.dump_ssa  op || Cmd.dump_all op )
                         ( Cmd.dump_hast op || Cmd.dump_all op )
                         ( Cmd.dump_hsc  op || Cmd.dump_all op )

        input = UniPath.fromUnixString $ Cmd.input op

    Initializer.initializeIfNeeded cfg

    build cfg op diag input
    return ()


build :: Config -> Cmd.Options -> Diagnostics -> UniPath -> IO ()
build cfg op diag filePath = do
    let name     = Cmd.libName    op
        version  = Cmd.libVersion op
        rootPath = case Cmd.rootPath op of
                        "" -> UniPath.basePath filePath
                        rp -> UniPath.fromUnixString rp
        libs     = Cmd.link op
    ghcFlags <- case Cmd.optimisation op of
                    0 -> pure ["-O0"]
                    1 -> pure ["-O1"]
                    2 -> pure ["-O2"]
                    _ -> fail "Unsupported optimisation level"
    let cabalFlags = case Cmd.global op of
                        True  -> ["--global"]
                        False -> []
        outputPath = UniPath.fromUnixString $ Cmd.output op
        buildType  = if Cmd.library op
                        then BuildConfig.Library
                        else BuildConfig.Executable outputPath
        bldCfg = BuildConfig name version libs ghcFlags cabalFlags buildType cfg diag
    ast <- Luna.runIO $ Build.parseFile rootPath filePath
    Luna.runIO $ Build.run bldCfg $ fst ast
    return ()
