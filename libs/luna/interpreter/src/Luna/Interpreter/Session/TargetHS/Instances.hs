---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.TargetHS.Instances where

import qualified GhcMonad
import qualified HscTypes
--import qualified GHC
--import qualified InstEnv
--import qualified Outputable
--import           Text.Show.Pretty             (ppShow)

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.Session (Session)




logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.TargetHS.Instances"


clean :: Session ()
clean = lift2 $

    --dflags <- GHC.getSessionDynFlags
    --GhcMonad.withSession $ \hscEnv -> do
    --    let instances = concatMap showPrepare $ fst $ HscTypes.ic_instances $ HscTypes.hsc_IC hscEnv
    --        dshow :: Outputable.Outputable a => a -> String
    --        dshow = Outputable.showSDoc dflags . Outputable.ppr
    --        showPrepare i = ppShow ( dshow $ InstEnv.is_cls_nm i
    --                        , dshow $ InstEnv.is_tcs    i
    --                        , dshow $ InstEnv.is_tvs    i
    --                        , dshow $ InstEnv.is_cls    i
    --                        , dshow $ InstEnv.is_tys    i
    --                        , dshow $ InstEnv.is_dfun   i
    --                        , dshow $ InstEnv.is_flag   i
    --                        ) ++ "\n" ++ dshow i ++ "\n--------------------------------------------------------------------\n"

    --    putStrLn  instances


    -- FIXME [PM] : Code below remove all declared instances. It may be
    --              dangerous and needs to be deeply tested or removed.
    GhcMonad.modifySession $ \hscEnv -> let
        ic = (HscTypes.hsc_IC hscEnv) {HscTypes.ic_instances = ([], []) }
        in hscEnv { HscTypes.hsc_IC = ic}
