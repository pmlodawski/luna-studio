---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}


module Flowbox.Luna.Data.Pass.ASTInfo where

import           Data.Map
import qualified Data.Map as Map

import Flowbox.Prelude hiding (id)


type ID = Int

data ASTInfo = ASTInfo { _lastID :: ID } deriving (Show)

makeLenses (''ASTInfo)


incID :: ASTInfo -> ASTInfo
incID = lastID %~ (+1)


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Default ASTInfo where
    def = ASTInfo 0
