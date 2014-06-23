---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Interpreter.Error where

import qualified Data.List                    as List
import qualified Language.Haskell.Interpreter as Interpreter

import Flowbox.Prelude           hiding (error)
import Flowbox.System.Log.Logger



format :: Interpreter.InterpreterError -> String
format err = case err of
    Interpreter.WontCompile ghcErrs -> "WontCompile:\n"
                                    ++ List.intercalate "\n\n"
                                       (map Interpreter.errMsg ghcErrs)
    _ -> show err


logErrors :: Interpreter.MonadIO m => LoggerIO -> Either Interpreter.InterpreterError a -> m ()
logErrors logger result = case result of
    Left err -> logger error $ format err
    _        -> return ()
