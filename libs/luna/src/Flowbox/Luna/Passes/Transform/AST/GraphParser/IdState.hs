---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.AST.GraphParser.IdState where

import           Control.Monad.State   

import           Flowbox.Prelude       



data IdState = IdState { varID :: Int }

type IdStateM m = MonadState IdState m


empty :: IdState
empty = IdState 0


newID :: IdStateM m => m Int
newID = do
    s <- get
    let num = varID s
    put $ s { varID = num + 1 }
    return num