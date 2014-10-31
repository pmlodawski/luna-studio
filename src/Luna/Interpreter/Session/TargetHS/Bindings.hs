---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.TargetHS.Bindings where

import qualified GHC
import qualified GhcMonad
import qualified HscTypes
import qualified Data.List as List

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.GHC.Util (dshow)
import Luna.Interpreter.Session.Session  (Session)
import Debug.Trace


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


bindingMatch :: GHC.DynFlags -> String -> HscTypes.TyThing -> Bool
bindingMatch dflags name binding = traceShow (name, bname, cname, result) result
    where 
      result = bname == name || cname == '.':name
      bname = dshow dflags $ HscTypes.tyThingAvailInfo binding
      cname = List.dropWhile (/= '.') bname


remove :: String -> Session ()
remove name = lift2 $ do
    dflags <- GHC.getSessionDynFlags
    GhcMonad.modifySession $ \hscEnv -> let
        hsc_IC       = HscTypes.hsc_IC       hscEnv
        ic_tythings  = HscTypes.ic_tythings hsc_IC
        ic_tythings' = filter (not . bindingMatch dflags name) ic_tythings
        hsc_IC'      = hsc_IC {HscTypes.ic_tythings = ic_tythings'}
        in hscEnv { HscTypes.hsc_IC = hsc_IC'}

 