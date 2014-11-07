---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.TargetHS.Bindings where

import qualified Data.List as List
import qualified GHC
import           GhcMonad  (GhcMonad)
import qualified GhcMonad
import qualified HscTypes
import qualified Linker

import Flowbox.Prelude                   hiding (matching)
import Flowbox.System.Log.Logger         as L
import Luna.Interpreter.Session.GHC.Util (dshow)



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


bindingMatch :: GHC.DynFlags -> String -> HscTypes.TyThing -> Bool
bindingMatch dflags name binding = result where
    result = bname == name || cname == '.':name
    bname = dshow dflags $ HscTypes.tyThingAvailInfo binding
    cname = List.dropWhile (/= '.') bname


remove :: GhcMonad m => String -> m ()
remove name = do
    dflags <- GHC.getSessionDynFlags
    hscEnv <- GhcMonad.getSession
    let ic_tythings = HscTypes.ic_tythings $ HscTypes.hsc_IC hscEnv
        matching    = filter (bindingMatch dflags name) ic_tythings

    logger info $ "Deleting " ++ show (length matching) ++ " bindings"
    GhcMonad.liftIO $ Linker.deleteFromLinkEnv $ map GHC.getName matching
    remove_ic_tythings name
    --GhcMonad.liftIO $ do
    --    pls <-  Linker.saveLinkerGlobals
    --    let closure_env = Linker.closure_env pls
    --    print $ dshow dflags closure_env


remove_ic_tythings :: GhcMonad m => String -> m ()
remove_ic_tythings name = do
    dflags <- GHC.getSessionDynFlags
    GhcMonad.modifySession $ \hscEnv -> let
        hsc_IC       = HscTypes.hsc_IC       hscEnv
        ic_tythings  = HscTypes.ic_tythings hsc_IC
        ic_tythings' = filter (not . bindingMatch dflags name) ic_tythings
        hsc_IC'      = hsc_IC {HscTypes.ic_tythings = ic_tythings'}
        in hscEnv { HscTypes.hsc_IC = hsc_IC'}
