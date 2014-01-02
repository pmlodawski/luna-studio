---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Analysis.ID.ExtractIDs where

import Data.IntSet (IntSet)

import           Flowbox.Luna.Data.AST.Zipper.Focus       (Focus)
import           Flowbox.Luna.Passes.Analysis.ID.State    (IDState)
import qualified Flowbox.Luna.Passes.Analysis.ID.State    as State
import qualified Flowbox.Luna.Passes.Analysis.ID.Traverse as IDTraverse
import           Flowbox.Luna.Passes.Pass                 (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                 as Pass
import           Flowbox.Prelude                          hiding (mapM, mapM_)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.ID.ExtractIDs"


type ExtractIDMonad m = PassMonad IDState m


run :: PassMonad s m => Focus -> Pass.Result m IntSet
run = (Pass.run_ (Pass.Info "ExtractIDs") $ State.make) . analyseFocus


analyseFocus :: ExtractIDMonad m => Focus -> Pass.Result m IntSet
analyseFocus m = do IDTraverse.traverseFocus State.appendID m
                    State.getIDs
