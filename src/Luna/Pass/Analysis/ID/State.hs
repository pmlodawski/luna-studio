---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Pass.Analysis.ID.State where

import           Control.Monad.State
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as IntSet

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common           as AST



logger :: Logger
logger = getLogger $(moduleName)


data IDState = IDState { _foundID :: AST.ID
                       , _ids   :: IntSet
                       } deriving (Show)

makeLenses ''IDState


type IDStateM m = MonadState IDState m


make :: IDState
make = IDState 0 IntSet.empty


getFoundID :: IDStateM m => m AST.ID
getFoundID = gets $ view foundID


setFoundID :: IDStateM m => AST.ID -> m ()
setFoundID i = modify $ foundID .~ i


findMaxID :: IDStateM m => AST.ID -> m ()
findMaxID i = do mi <- getFoundID
                 when (i > mi) $ setFoundID i

findMinID :: IDStateM m => AST.ID -> m ()
findMinID i = do mi <- getFoundID
                 when (i < mi) $ setFoundID i


getIDs :: IDStateM m => m IntSet
getIDs = gets $ view ids


setIDs :: IDStateM m => IntSet -> m ()
setIDs i = modify $ ids .~ i 


appendID :: IDStateM m => AST.ID -> m ()
appendID i = getIDs >>= (setIDs . IntSet.insert i)
