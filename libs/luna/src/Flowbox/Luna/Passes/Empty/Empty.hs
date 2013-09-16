---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Flowbox.Luna.Passes.Empty.Empty where

import qualified Flowbox.Luna.Passes.Pass  as Pass
import           Flowbox.Luna.Passes.Pass    (PassMonad)

import           Flowbox.System.Log.Logger   
import           Flowbox.Prelude           hiding (error, id)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Empty.Empty"


type SSAMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => a -> Pass.Result m a
run = (Pass.run_ Pass.NoState) . pass


pass :: SSAMonad m => a -> Pass.Result m a
pass a = return a

