---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Flowbox.Luna.Passes.Transform.AST.Desugar.State where

import           Control.Monad.State            (MonadState, get, modify, put)
import qualified Control.Monad.State            as State
import qualified Data.IntMap                    as IntMap
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Flowbox.Luna.Data.Pass.ASTInfo (ASTInfo)
import qualified Flowbox.Luna.Data.Pass.ASTInfo as ASTInfo

import Flowbox.Prelude           hiding (id)
import Flowbox.System.Log.Logger hiding (info)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.AST.Desugar.State"

type ID = Int

data DesugarState = DesugarState { _info :: ASTInfo
                                 }
                  deriving (Show)

makeLenses (''DesugarState)

type DesugarMonad m = (MonadState DesugarState m, Applicative m)


getInfo :: DesugarMonad m => m ASTInfo
getInfo = view info <$> get

incID :: DesugarMonad m => m ()
incID = modify ((info . ASTInfo.lastID) %~ (+1))

genID :: DesugarMonad m => m ID
genID = incID *> (view (info . ASTInfo.lastID) <$> get)


mk :: ASTInfo -> DesugarState
mk info = DesugarState info

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Default DesugarState where
    def = DesugarState def
