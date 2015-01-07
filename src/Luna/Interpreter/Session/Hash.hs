---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Hash where

import qualified Data.Maybe as Maybe

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.Interpreter.Session.Data.Hash    (Hash)
import           Luna.Interpreter.Session.Data.VarName (VarName)
import qualified Luna.Interpreter.Session.Data.VarName as VarName
import           Luna.Interpreter.Session.Session      (Session)
import qualified Luna.Interpreter.Session.Session      as Session



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


compute :: String -> Session mm (Maybe Hash)
compute varName = Session.interpret $ "hash " ++ varName


computeInherit :: String -> [VarName] -> Session mm [Hash]
computeInherit varName varNames =
    Maybe.maybe inherited (:inherited) <$> compute varName
    where inherited = concatMap (view VarName.hash) varNames
