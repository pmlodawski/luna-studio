---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

module Luna.Interpreter.Session.TargetHS.TargetHS where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified DynFlags as GHC

import           Flowbox.Prelude                             hiding (perform)
import           Flowbox.System.Log.Logger
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session
import qualified Luna.Interpreter.Session.TargetHS.Generator as Generator
import qualified Luna.Interpreter.Session.TargetHS.Instances as Instances
import qualified Luna.Interpreter.Session.TargetHS.Reload    as Reload



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.TargetHS.Reload"


enabledFlags :: [GHC.ExtensionFlag]
enabledFlags = [ GHC.Opt_DataKinds
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


disabledFlags :: [GHC.ExtensionFlag]
disabledFlags = [GHC.Opt_MonomorphismRestriction]


runDecls :: [String] -> Session ()
runDecls = Session.withFlags enabledFlags disabledFlags . mapM_ Session.runDecls


reloadAll :: Session ()
reloadAll = Instances.cleanAll
         >> Generator.genAll >>= runDecls


reloadFunctions :: Session ()
reloadFunctions = Instances.cleanFunctions
               >> Generator.genFunctions >>= runDecls


reloadClass :: DefPoint -> Session ()
reloadClass defPoint = Instances.cleanFunctions
                    >> Generator.genClass defPoint >>= runDecls


reload :: Session ()
reload = do
    reloads <- Session.getReloads
    logger debug $ "Reloading: " ++ show reloads
    let perform (libID, Reload.ReloadClasses items) =
            mapM_ (reloadClass . DefPoint libID . view Reload.breadcrumbs) (Set.toList items) >> reloadFunctions
        perform (libID, Reload.ReloadFunctions  ) = reloadFunctions
        perform (libID, Reload.ReloadLibrary    ) = reloadAll
        perform (libID, Reload.NoReload         ) = return ()
    mapM_ perform $ Map.toList reloads
    Session.cleanReloads

