---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Hint.Typecheck where

import           Control.Monad                             ((>=>))
import qualified GHC

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.Interpreter.Session.Hint.Conversions as Conversions



logger :: LoggerIO
logger = getLoggerIO $moduleName


typeOf :: GHC.GhcMonad m => String -> m String
typeOf = GHC.exprType >=> Conversions.typeToString


debugType :: GHC.GhcMonad m => String -> m ()
debugType v = do
    t <- typeOf v
    putStrLn $ v <> "\n    :: " <> t
