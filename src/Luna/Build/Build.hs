---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Luna.Build.Build where

import Control.Applicative

import           Flowbox.Config.Config           (Config)
import           Flowbox.Control.Error
import qualified Flowbox.Initializer.Initializer as Initializer
import           Flowbox.Prelude                 hiding (op)
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform         as Platform
import           Flowbox.System.UniPath          (UniPath)
import qualified Flowbox.System.UniPath          as UniPath
import qualified Luna.Build.Cmd                  as Cmd
import qualified Luna.Pass.Build.Build           as Build
import           Luna.Pass.Build.BuildConfig     (BuildConfig (BuildConfig))
import qualified Luna.Pass.Build.BuildConfig     as BuildConfig
import           Luna.Pass.Build.Diagnostics     (Diagnostics (Diagnostics))



logger :: Logger
logger = getLogger "Flowbox.Lunac.Build"



run :: Config -> Cmd.Options -> IO ()
run cfg op = do
    let diag = Diagnostics
                         ( Cmd.dump_ast  op || Cmd.dump_all op )
                         ( Cmd.dump_aa   op || Cmd.dump_all op )
                         ( Cmd.dump_ssa  op || Cmd.dump_all op )
                         ( Cmd.dump_hash op || Cmd.dump_all op )
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
    ghcFlags' <- case Cmd.optimisation op of
                    0 -> pure ["-O0"]
                    1 -> pure ["-O1"]
                    2 -> pure ["-O2"]
                    _ -> fail "Unsupported optimisation level"
    let ccFlags  = ["\"-DDEBUG\"" | Cmd.ddebug op]
        -- FIXME[pm]: Why it BLOWS UP when we enable -Wall?
        ghcFlags = ["-threaded", "-Odph", "-optlo-O3"] ++ ["-DDEBUG" | Cmd.ddebug op]

        cabalFlags = ["--global" | Cmd.global op ]
        outputPath = Platform.addExeOnWindows $ UniPath.fromUnixString $ Cmd.output op
        buildType  = if Cmd.library op
                        then BuildConfig.Library
                        else BuildConfig.Executable outputPath
        buildDir   = case Cmd.buildDir op of
                        "" -> Nothing
                        d  -> Just $ UniPath.fromUnixString d
        bldCfg = BuildConfig name version libs ghcFlags ccFlags cabalFlags buildType cfg diag buildDir
    (ast, _, astInfo) <- eitherStringToM =<< Build.parseFile rootPath filePath
    eitherStringToM =<< Build.run bldCfg ast astInfo True
