---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Analysis.MaxID.State where

import Control.Monad.State

import qualified Flowbox.Luna.Data.AST.Utils as AST
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.MaxID.State"


data MaxIDState = MaxIDState { maxID :: AST.ID
                            } deriving (Show)


type MaxIDStateM m = MonadState MaxIDState m


make :: MaxIDState
make = MaxIDState 0


getMaxID :: MaxIDStateM m => m AST.ID
getMaxID = get >>= return . maxID


setMaxID :: MaxIDStateM m => AST.ID -> m ()
setMaxID i = do s <- get
                put s { maxID = i }


reportID :: MaxIDStateM m => AST.ID -> m ()
reportID i = do mi <- getMaxID
                if i > mi
                    then setMaxID i
                    else return ()
