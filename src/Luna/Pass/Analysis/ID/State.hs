---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Luna.Pass.Analysis.ID.State where

import           Control.Monad.State
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as IntSet

import qualified Luna.AST.Common as AST
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.ID.State"


data IDState = IDState { maxID :: AST.ID
                       , ids   :: IntSet
                       } deriving (Show)


type IDStateM m = MonadState IDState m


make :: IDState
make = IDState 0 IntSet.empty


getMaxID :: IDStateM m => m AST.ID
getMaxID = get >>= return . maxID


setMaxID :: IDStateM m => AST.ID -> m ()
setMaxID i = do s <- get
                put s { maxID = i }


compareID :: IDStateM m => AST.ID -> m ()
compareID i = do mi <- getMaxID
                 if i > mi
                    then setMaxID i
                    else return ()


getIDs :: IDStateM m => m IntSet
getIDs = get >>= return . ids


setIDs :: IDStateM m => IntSet -> m ()
setIDs i = do s <- get
              put s { ids = i }


appendID :: IDStateM m => AST.ID -> m ()
appendID i = getIDs >>= (setIDs . IntSet.insert i)
