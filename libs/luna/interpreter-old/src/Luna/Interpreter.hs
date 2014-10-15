---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter where

import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad           (forever)
import           DynFlags                (PkgConfRef (PkgConfFile))
import qualified DynFlags                as GHC
import           GHC                     (Ghc, GhcMonad)
import qualified GHC                     as GHC
import           MonadUtils              (liftIO)

import           Flowbox.Config.Config     (Config)
import qualified Flowbox.Config.Config     as Config
import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


initialize :: GhcMonad m => Config -> m ()
initialize config = do
    let isNotUser GHC.UserPkgConf = False
        isNotUser _ = True
        extraPkgConfs p = [ GHC.PkgConfFile $ Config.pkgDb $ Config.global config
                          , GHC.PkgConfFile $ Config.pkgDb $ Config.local config
                          ] ++ filter isNotUser p
    flags <- GHC.getSessionDynFlags
    _ <- GHC.setSessionDynFlags flags
                { GHC.extraPkgConfs = extraPkgConfs
                , GHC.hscTarget = GHC.HscInterpreted
                , GHC.ghcLink   = GHC.LinkInMemory
                --, GHC.verbosity = 4
                }
    return ()


setHardodedExtensions :: GhcMonad m => m ()
setHardodedExtensions = do
    flags <- GHC.getSessionDynFlags
    let f = foldl GHC.xopt_set flags [ GHC.Opt_DataKinds
                                     , GHC.Opt_DeriveDataTypeable
                                     , GHC.Opt_DeriveGeneric
                                     , GHC.Opt_DysfunctionalDependencies
                                     , GHC.Opt_FlexibleContexts
                                     , GHC.Opt_FlexibleInstances
                                     , GHC.Opt_GADTs
                                     , GHC.Opt_RebindableSyntax
                                     , GHC.Opt_TemplateHaskell
                                     , GHC.Opt_UndecidableInstances

                                     , GHC.Opt_MultiParamTypeClasses
                                     ]
        f' = foldl GHC.xopt_unset f  [ GHC.Opt_MonomorphismRestriction]
    _ <- GHC.setSessionDynFlags f'
    return ()


setImports :: GhcMonad m => [String] -> m ()
setImports = GHC.setContext . map (GHC.IIDecl . GHC.simpleImportDecl . GHC.mkModuleName)


compileAndRun :: GhcMonad m => [String] -> String -> String -> m ()
compileAndRun imports declarations stmt = do
    GHC.setTargets []
    GHC.load GHC.LoadAllTargets
    setImports imports
    _  <- GHC.runDecls declarations
    rr <- GHC.runStmt stmt GHC.RunToCompletion
    case rr of
            GHC.RunOk _        -> liftIO $ logger info "runOk"
            GHC.RunException e -> do let errStr = "Exception: " ++ (show e)
                                     liftIO $ logger error errStr
                                     fail errStr
            GHC.RunBreak {}    -> liftIO $ logger info "runBreak"
    return ()


run :: Config -> Ghc a -> IO a
run config r = GHC.runGhc (Just $ Config.topDir $ Config.ghcS config) r


runSource :: Config -> [String] -> String -> String -> IO ()
runSource config imports declarations stmt =
    run config $ do initialize config
                    setHardodedExtensions
                    compileAndRun imports declarations stmt


channelLoop :: GhcMonad m => Chan String -> [String] -> m ()
channelLoop chan imports = forever $ do
    declarations <- liftIO $ Chan.readChan chan
    compileAndRun imports declarations "main"
    return ()


runChannelLoop :: Config -> Chan String -> [String] -> IO ()
runChannelLoop config chan imports = run config $ channelLoop chan imports

