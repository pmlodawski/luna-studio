---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.TargetHS.TargetHS where

import qualified Data.Map                                    as Map
import qualified Data.Set                                    as Set
import qualified DynFlags                                    as GHC

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session
import qualified Luna.Interpreter.Session.TargetHS.Generator as Generator
import qualified Luna.Interpreter.Session.TargetHS.Instances as Instances
import qualified Luna.Interpreter.Session.TargetHS.Reload    as Reload



logger :: LoggerIO
logger = getLoggerIO $moduleName


enabledFlags :: [GHC.ExtensionFlag]
enabledFlags = [ GHC.Opt_Cpp
               , GHC.Opt_DataKinds
               , GHC.Opt_DeriveDataTypeable
               , GHC.Opt_DeriveGeneric
               , GHC.Opt_DysfunctionalDependencies
               , GHC.Opt_ExtendedDefaultRules
               , GHC.Opt_FlexibleContexts
               , GHC.Opt_FlexibleInstances
               , GHC.Opt_MultiParamTypeClasses
               , GHC.Opt_RebindableSyntax
               , GHC.Opt_TemplateHaskell
               , GHC.Opt_TypeFamilies
               , GHC.Opt_UndecidableInstances
               , GHC.Opt_ViewPatterns
               ]


disabledFlags :: [GHC.ExtensionFlag]
disabledFlags = [GHC.Opt_MonomorphismRestriction]


runDecls :: String -> Session mm ()
runDecls = Session.withExtensionFlags enabledFlags disabledFlags . Session.runDecls


reloadAll :: Session mm ()
reloadAll = Session.atomically $ Instances.cleanAll
                             >>  Generator.genAll
                             >>= runDecls


reloadFunctions :: Session mm ()
reloadFunctions = return ()
                  --Session.atomically $ Instances.cleanFunctions
                  --                 >>  Generator.genFunctions
                  --                 >>= runDecls


reloadClass :: DefPoint -> Session m ()
reloadClass defPoint = Session.atomically $ Instances.cleanFunctions
                                        >>  Generator.genClass defPoint
                                        >>= runDecls


reload :: Session mm ()
reload = Env.fragile $ do
    reloads <- Env.getReloads
    logger debug $ "Reloading: " ++ show reloads
    let perform (libID, Reload.ReloadClasses items) =
            mapM_ (reloadClass . DefPoint libID . view Reload.breadcrumbs) (Set.toList items) >> reloadFunctions
        perform (libID, Reload.ReloadFunctions  ) = reloadFunctions
        perform (libID, Reload.ReloadLibrary    ) = reloadAll
        perform (libID, Reload.NoReload         ) = return ()
    mapM_ perform $ Map.toList reloads
    Env.cleanReloads

