---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Compiler.Build where

import Control.Applicative

import           Flowbox.Config.Config           (Config)
import qualified Flowbox.Config.Config           as Config
import qualified Flowbox.Initializer.Initializer as Initializer
import           Flowbox.Prelude                 hiding (op)
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform         as Platform
import           Flowbox.System.UniPath          (UniPath)
import qualified Flowbox.System.UniPath          as UniPath
import qualified Luna.Build.Build                as Build
import           Luna.Build.BuildConfig          (BuildConfig (BuildConfig))
import qualified Luna.Build.BuildConfig          as BuildConfig
import           Luna.Build.Diagnostics          (Diagnostics (Diagnostics))
import           Luna.Console.Options            (Options)
import qualified Luna.Console.Options            as Options



logger :: Logger
logger = getLogger $moduleName



run :: Config -> Options -> IO ()
run cfg op = do
    let diag = Diagnostics
                         ( Options.dump_ast  op || Options.dump_all op )
                         ( Options.dump_aa   op || Options.dump_all op )
                         ( Options.dump_ssa  op || Options.dump_all op )
                         ( Options.dump_hash op || Options.dump_all op )
                         ( Options.dump_hast op || Options.dump_all op )
                         ( Options.dump_hsc  op || Options.dump_all op )

        input = UniPath.fromUnixString $ Options.input op
    Initializer.initializeIfNeeded cfg
    build cfg op diag input


build :: Config -> Options.Options -> Diagnostics -> UniPath -> IO ()
build cfg op diag filePath = do
    let name     = Options.libName    op
        version  = Options.libVersion op
        rootPath = case Options.rootPath op of
                        "" -> UniPath.basePath filePath
                        rp -> UniPath.fromUnixString rp
        libs     = Options.link op
    ghcOptions' <- case Options.optimisation op of
                    0 -> pure ["-O0"]
                    1 -> pure ["-O1"]
                    2 -> pure ["-O2"]
                    _ -> fail "Unsupported optimisation level"
    let ccOptions  = ["\"-DDEBUG\"" | Options.ddebug op]
        includeDirs = [Config.path $ Config.templates cfg]
        -- FIXME[pm]: Why it BLOWS UP when we enable -Wall?
        ghcOptions = ["-threaded", "-Odph", "-optlo-O3"] ++ ["-DDEBUG" | Options.ddebug op] ++ ghcOptions'

        cabalFlags = ["--global" | Options.global op ]
        outputPath = Platform.addExeOnWindows $ UniPath.fromUnixString $ Options.output op
        buildType  = if Options.library op
                        then BuildConfig.Library
                        else BuildConfig.Executable outputPath
        buildDir   = case Options.buildDir op of
                        "" -> Nothing
                        d  -> Just $ UniPath.fromUnixString d
        inclStd    = not $ Options.noStdlib  op
        bldCfg     = BuildConfig name version libs ghcOptions ccOptions includeDirs cabalFlags buildType cfg diag buildDir inclStd
    Build.run bldCfg diag rootPath filePath
