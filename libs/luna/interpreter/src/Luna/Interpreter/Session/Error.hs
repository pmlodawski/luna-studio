---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Luna.Interpreter.Session.Error where

import Control.Monad.IO.Class (MonadIO)

import Flowbox.Prelude           hiding (error)
import Flowbox.System.Log.Logger


type ErrorStr = String

data Error = OtherError ErrorStr
           deriving (Show)



format :: Error -> String
format err = case err of
    _   -> show err


logErrors :: MonadIO m => LoggerIO -> Either Error a -> m ()
logErrors logger result = case result of
    Left err -> logger error $ format err
    _        -> return ()
