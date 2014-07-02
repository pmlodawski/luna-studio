---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Interpreter.Session.Error where

import qualified Data.List                    as List
import qualified Language.Haskell.Interpreter as Interpreter

import Flowbox.Prelude           hiding (error)
import Flowbox.System.Log.Logger



type ErrorStr = String

data Error = InterpreterError Interpreter.InterpreterError
           | OtherError ErrorStr
           deriving (Show)



format :: Error -> String
format err = case err of
    InterpreterError (Interpreter.WontCompile ghcErrs)
        -> "WontCompile:\n" ++ List.intercalate "\n\n" (map Interpreter.errMsg ghcErrs)
    _   -> show err


logErrors :: Interpreter.MonadIO m => LoggerIO -> Either Error a -> m ()
logErrors logger result = case result of
    Left err -> logger error $ format err
    _        -> return ()
