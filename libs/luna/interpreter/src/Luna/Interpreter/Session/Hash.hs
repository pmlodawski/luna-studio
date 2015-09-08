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
import           Luna.Interpreter.Session.Data.Hash (Hash)
import           Luna.Interpreter.Session.Session   (Session)
import qualified Luna.Interpreter.Session.Session   as Session



logger :: LoggerIO
logger = getLoggerIO $moduleName


compute :: String -> Session mm (Maybe Hash)
compute expression =
    liftIO =<< Session.interpret ("hash " <> expression)


computeInherit :: String -> [[Hash]] -> Session mm [Hash]
computeInherit expression inherited =
    if any null inherited
        then return []
        else Maybe.maybe hashes (:hashes) <$> compute expression
    where
        hashes = concat inherited
    --Maybe.maybe inherited (:inherited) <$> compute varName
    --where inherited = concatMap (view VarName.hash) hashes
