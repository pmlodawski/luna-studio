---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.TargetHS.Instances where

import qualified FamInstEnv
import qualified GHC
import qualified GhcMonad
import qualified HscTypes
import qualified InstEnv
import           Text.Show.Pretty (ppShow)

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.GHC.Util (dshow)
import Luna.Interpreter.Session.Session  (Session)



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type ClsInstSelector = GHC.DynFlags -> InstEnv.ClsInst -> Bool
type FamInstSelector = GHC.DynFlags -> FamInstEnv.FamInst -> Bool


cleanFunctions :: Session ()
cleanFunctions = clean clsInstFilter famInstFilter where
    clsInstFilter dflags inst = dshow dflags (InstEnv.is_cls_nm inst) /= "Luna.Target.HS.Data.Func.Func.Func"
                             && dshow dflags (InstEnv.is_cls_nm inst) /= "Luna.Target.HS.Data.Struct.Mem.HasMem"
    famInstFilter = const $ const True


cleanAll :: Session ()
cleanAll = clean true true where
    true = const $ const True


clean :: ClsInstSelector -> FamInstSelector -> Session ()
clean clsInstFilter famInstFilter = lift2 $ do
    dflags <- GHC.getSessionDynFlags

    -- FIXME [PM] : Code below remove all declared instances. It may be
    --              dangerous and needs to be deeply tested or removed.
    GhcMonad.modifySession $ \hscEnv -> let
        hsc_IC             = HscTypes.hsc_IC       hscEnv
        (clsInst, famInst) = HscTypes.ic_instances hsc_IC
        clsInst'           = filter (clsInstFilter dflags) clsInst
        famInst'           = filter (famInstFilter dflags) famInst

        hsc_IC'            = hsc_IC {HscTypes.ic_instances = (clsInst', famInst')}
        in hscEnv { HscTypes.hsc_IC = hsc_IC'}


printInstances :: Session ()
printInstances = lift2 $ do
    dflags <- GHC.getSessionDynFlags
    GhcMonad.withSession $ \hscEnv -> do
        let instances     = HscTypes.ic_instances $ HscTypes.hsc_IC hscEnv

            clsInstances  = concatMap clsInstShow $ fst instances
            clsInstShow i = "\n--- class instance -------------------------------------------------\n" ++
                            ppShow
                            ( dshow dflags $ InstEnv.is_cls_nm i
                            , dshow dflags $ InstEnv.is_tcs    i
                            , dshow dflags $ InstEnv.is_tvs    i
                            , dshow dflags $ InstEnv.is_cls    i
                            , dshow dflags $ InstEnv.is_tys    i
                            , dshow dflags $ InstEnv.is_dfun   i
                            , dshow dflags $ InstEnv.is_flag   i
                            ) ++ "\n" ++ dshow dflags i

            famInstances  = concatMap famInstShow $ snd instances
            famInstShow i = "\n--- family instance ------------------------------------------------\n" ++
                            ppShow
                            ( dshow dflags $ FamInstEnv.fi_axiom  i
                            --, dshow dflags $ FamInstEnv.fi_flavor i
                            , dshow dflags $ FamInstEnv.fi_fam    i
                            , dshow dflags $ FamInstEnv.fi_tcs    i
                            , dshow dflags $ FamInstEnv.fi_tvs    i
                            , dshow dflags $ FamInstEnv.fi_tys    i
                            , dshow dflags $ FamInstEnv.fi_rhs    i

                            ) ++ "\n" ++ dshow dflags i

        putStrLn  famInstances
        putStrLn  clsInstances
